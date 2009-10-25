;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;
;;; berkeley-db.lisp -- FFI interface to Berkeley DB
;;; 
;;; Initial version 9/10/2004 by Ben Lee
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

(declaim 
 #-elephant-without-optimize (optimize (speed 3) (safety 0))
 #-lispworks
 (inline %db-get-key-buffered db-get-key-buffered 
		 %db-get-buffered db-get-buffered db-get 
		 %db-put-buffered db-put-buffered 
		 %db-put db-put 
		 %db-delete db-delete-buffered db-delete 
		 %db-delete-kv db-delete-kv-buffered
		 %db-cursor db-cursor %db-cursor-close db-cursor-close
		 %db-cursor-duplicate db-cursor-duplicate
		 %db-cursor-get-key-buffered 
		 db-cursor-move-buffered
		 db-cursor-set-buffered
		 db-cursor-get-both-buffered
		 %db-cursor-pget-key-buffered 
		 db-cursor-pmove-buffered
		 db-cursor-pset-buffered
		 db-cursor-pget-both-buffered
		 %db-cursor-put-buffered db-cursor-put-buffered
		 %db-cursor-delete db-cursor-delete
		 %db-txn-begin db-transaction-begin
		 %db-txn-abort db-transaction-abort
		 %db-txn-commit db-transaction-commit
		 %db-transaction-id 
		 %db-sequence-get db-sequence-get
		 %db-sequence-get-lower db-sequence-get-fixnum
		 ))

;;
;; EXTERNAL LIBRARY DEPENDENCIES - LOAD DURING LOAD/COMPILATION
;;

(eval-when (:compile-toplevel :load-toplevel)

  (def-function ("db_strerr" %db-strerror)
      ((error :int))
    :returning :cstring)

  (defun db-strerror (errno)
    "Get the string error associated with an error number."
    (convert-from-cstring (%db-strerror errno)))

  (define-condition db-error (error) 
    ((errno :type fixnum :initarg :errno :reader db-error-errno))
    (:report
     (lambda (condition stream)
       (declare (type db-error condition) (type stream stream))
       (format stream "Berkeley DB error: ~A"
	       (db-strerror (db-error-errno condition)))))
    (:documentation "Berkeley DB errors."))

  )

(defmacro txn-default (dvar)
  `(progn
     (assert (null ,dvar))
     +NULL-VOID+))

;;
;; Constants and Flags
;; eventually write a macro which generates a custom flag function.
;;

;; Current header file version required: Berkeley DB 4.5

;; I don't like the UFFI syntax for enumerations
(defconstant DB-BTREE                 1)
(defconstant DB-HASH                  2)
(defconstant DB-RECNO                 3)
(defconstant DB-QUEUE                 4)
(defconstant DB-UNKNOWN               5)

(defconstant DB_LOCK_NOWAIT   #x00000002)

(defconstant DB_CREATE        #x00000001)
(defconstant DB_FORCE         #x00000004)
(defconstant DB_MULTIVERSION  #x00000008)
(defconstant DB_NOMMAP        #x00000010)
(defconstant DB_RDONLY        #x00000020)
(defconstant DB_RECOVER       #x00000040)
(defconstant DB_THREAD        #x00000080)
(defconstant DB_TRUNCATE      #x00000100)
(defconstant DB_TXN_NOSYNC    #x00000200)
(defconstant DB_TXN_NOT_DURABLE #x00000400)
(defconstant DB_TXN_WRITE_NOSYNC #x00000800)

(defconstant DB_EXCL          #x00004000)

(defconstant DB_TXN_NOWAIT    #x00004000)
(defconstant DB_TXN_SYNC      #x00008000)

(defconstant DB_DUP           #x00008000)
(defconstant DB_DUPSORT       #x00010000)

(defconstant DB_JOINENV          #x00000000)
(defconstant DB_INIT_CDB         #x00004000)
(defconstant DB_INIT_LOCK        #x00008000)
(defconstant DB_INIT_LOG         #x00010000)
(defconstant DB_INIT_MPOOL       #x00020000)
(defconstant DB_INIT_REP         #x00040000)
(defconstant DB_INIT_TXN         #x00080000)
(defconstant DB_LOCKDOWN         #x00100000)
(defconstant DB_PRIVATE          #x00200000)
(defconstant DB_RECOVER_FATAL    #x00400000)
(defconstant DB_REGISTER         #x00800000)
(defconstant DB_SYSTEM_MEM       #x01000000)
(defconstant DB_AUTO_COMMIT      #x02000000)
(defconstant DB_READ_COMMITTED   #x04000000)
(defconstant DB_DEGREE_2         #x04000000) ;; DEPRECATED, now called DB_READ_COMMITTED
(defconstant DB_READ_UNCOMMITTED #x08000000)
(defconstant DB_DIRTY_READ       #x08000000) ;; DEPRECATED, now called DB_READ_UNCOMMITTED

(defconstant DB_AFTER		      1)
(defconstant DB_BEFORE		      3)
(defconstant DB_CURRENT		      6)
(defconstant DB_FIRST		      7)
(defconstant DB_GET_BOTH	      8)
(defconstant DB_GET_BOTH_RANGE	     10)
(defconstant DB_KEYFIRST	     13)
(defconstant DB_KEYLAST		     14)
(defconstant DB_LAST		     15)
(defconstant DB_NEXT		     16)
(defconstant DB_NEXT_DUP	     17)
(defconstant DB_NEXT_NODUP	     18)
(defconstant DB_PREV		     23)
(defconstant DB_PREV_NODUP	     24)
(defconstant DB_SET		     25)
(defconstant DB_SET_RANGE	     27)

(defconstant DB_NODUPDATA	     19)
(defconstant DB_NOOVERWRITE	     20)
(defconstant DB_NOSYNC		     21)

(defconstant DB_POSITION	     22)

(defconstant DB_SEQ_DEC	     #x00000001)
(defconstant DB_SEQ_INC	     #x00000002)
(defconstant DB_SEQ_WRAP     #x00000008)

(defconstant DB_SET_LOCK_TIMEOUT     26)
(defconstant DB_SET_TXN_TIMEOUT      30)

(defconstant DB_FREELIST_ONLY  #x00004000)
(defconstant DB_FREE_SPACE     #x00008000)

(defconstant DB_KEYEMPTY         -30997)
(defconstant DB_KEYEXIST	 -30996)
(defconstant DB_LOCK_DEADLOCK    -30995)
(defconstant DB_LOCK_NOTGRANTED  -30994)
(defconstant DB_NOTFOUND         -30989)

(defconstant DB_LOCK_DEFAULT	     1)
(defconstant DB_LOCK_EXPIRE	     2)
(defconstant DB_LOCK_MAXLOCKS        3)
(defconstant DB_LOCK_MAXWRITE        4)
(defconstant DB_LOCK_MINLOCKS        5)
(defconstant DB_LOCK_MINWRITE        6)
(defconstant DB_LOCK_OLDEST	     7)
(defconstant DB_LOCK_RANDOM	     8)
(defconstant DB_LOCK_YOUNGEST        9)


(def-enum DB-LOCKOP ((:DUMP 0) :GET :GET-TIMEOUT :INHERIT 
		     :PUT :PUT-ALL :PUT-OBJ :PUT-READ
		     :TIMEOUT :TRADE :UPGRADE-WRITE))

(def-enum DB-LOCKMODE ((:NG 0) :READ :WRITE :WAIT 
		       :IWRITE :IREAD :IWR :DIRTY :WWRITE))

(def-struct DB-LOCK
    (off :unsigned-int)
  (ndx :unsigned-int)
  (gen :unsigned-int)
  (mode DB-LOCKMODE))

#+openmcl
(ccl:def-foreign-type DB-LOCK (:struct DB-LOCK))

(def-struct DB-LOCKREQ
    (op DB-LOCKOP)
  (mode DB-LOCKMODE)
  (timeout :unsigned-int)
  (obj (:array :char))
  (lock (* DB-LOCK)))

#+openmcl
(ccl:def-foreign-type DB-LOCKREQ (:struct DB-LOCKREQ))

(defconstant +2^32+ 4294967296)
(defconstant +2^64+ 18446744073709551616)
(defconstant +2^32-1+ (1- +2^32+))

(defmacro make-64-bit-integer (high32 low32)
  `(+ ,low32 (ash ,high32 32)))

(defmacro high32 (int64)
  `(ash ,int64 -32))

(defmacro low32 (int64)
  `(logand ,int64 +2^32-1+))

(defmacro split-64-bit-integer (int64)
  `(values (ash ,int64 -32) (logand ,int64 +2^32-1+)))

;; Wrapper macro -- handles errno return values
;; makes flags into keywords
;; makes keyword args, cstring wrappers

(eval-when (:compile-toplevel)
  (defun make-wrapper-args (args flags keys)
    (if (or flags keys)
	(append (remove-keys (remove 'flags args) keys)
		`(&key ,@flags ,@keys))
	(remove 'flags args)))
  
  (defun remove-keys (args keys)
    (if keys
	(loop for key in keys
	      for kw = (if (atom key) key (first key))
	      for wrapper-args = (remove kw args) then (remove kw wrapper-args)
	      finally (return wrapper-args))
	args))
  
  (defun make-fun-args (args flags)
    (if flags
	(substitute (cons 'flags (symbols-to-kw-pairs flags)) 'flags args)
	(substitute 0 'flags args)))
  
  (defun make-out-args (count)
    (loop for i from 1 to count
	  collect (gensym)))
  
  (defun symbols-to-kw-pairs (symbols)
    (loop for symbol in symbols
	  append (list (intern (symbol-name symbol) "KEYWORD")
		       symbol)))
  
  (defun symbols-to-pairs (symbols)
    (loop for symbol in symbols
	  collect (list symbol symbol)))
  )

(defmacro wrap-errno (names args &key (keys nil) (flags nil)
		      (cstrings nil) (outs 1) (declarations nil)
		      (documentation nil)
		      (transaction nil))
  (let ((wname (if (listp names) (first names) names))
	(fname (if (listp names) (second names)
		   (intern (concatenate 'string "%" (symbol-name names)))))
	(wrapper-args (make-wrapper-args args flags keys))
	(fun-args (make-fun-args args flags))
	(errno (gensym)))
    (if (> outs 1)
	(let ((out-args (make-out-args outs)))
	  `(defun ,wname ,wrapper-args
	    ,@(if documentation (list documentation) (values))
	    ,@(if declarations (list declarations) (values))	    
	    (with-cstrings ,(symbols-to-pairs cstrings)
	      (multiple-value-bind ,out-args
		  (,fname ,@fun-args)
		(let ((,errno ,(first out-args)))
		  (declare (type fixnum ,errno))
		  (cond
		    ((= ,errno 0) (values ,@(rest out-args)))
		    ,@(if transaction
			  (list `((or (= ,errno DB_LOCK_DEADLOCK)
				      (= ,errno DB_LOCK_NOTGRANTED))
				  (throw 'transaction ,transaction)))
			  (values))
		    (t (error 'db-error :errno ,errno))))))))
	`(defun ,wname ,wrapper-args
	  ,@(if documentation (list documentation) (values))
	  ,@(if declarations (list declarations) (values))
	  (with-cstrings ,(symbols-to-pairs cstrings)
	    (let ((,errno (,fname ,@fun-args)))
	      (declare (type fixnum ,errno))
	      (cond 
		((= ,errno 0) nil)
		,@(if transaction
		      (list `((or (= ,errno DB_LOCK_DEADLOCK)
			       (= ,errno DB_LOCK_NOTGRANTED))
			      (throw 'transaction ,transaction)))
		      (values))
		(t (error 'db-error :errno ,errno)))))))))

(defmacro flags (&key auto-commit joinenv init-cdb init-lock init-log
		 init-mpool init-rep init-txn recover recover-fatal lockdown
		 private system-mem thread force create excl nommap 
		 degree-2 read-committed dirty-read read-uncommitted
		 rdonly truncate txn-nosync txn-nowait txn-sync lock-nowait
		 dup dup-sort current first get-both get-both-range last next
		 next-dup next-nodup prev prev-nodup set set-range
		 after before keyfirst keylast freelist-only free-space
		 no-dup-data no-overwrite nosync position 
		 seq-dec seq-inc seq-wrap set-lock-timeout
		 set-transaction-timeout)
  (let ((flags (gensym)))
    `(let ((,flags 0))
      (declare (type fixnum ,flags))
      ,@(when auto-commit `((when ,auto-commit (setq ,flags (logior ,flags DB_AUTO_COMMIT)))))
      ,@(when joinenv `((when ,joinenv (setq ,flags (logior ,flags DB_JOINENV)))))
      ,@(when init-cdb `((when ,init-cdb (setq ,flags (logior ,flags DB_INIT_CDB)))))
      ,@(when init-lock `((when ,init-lock (setq ,flags (logior ,flags DB_INIT_LOCK)))))
      ,@(when init-log `((when ,init-log (setq ,flags (logior ,flags DB_INIT_LOG)))))
      ,@(when init-mpool `((when ,init-mpool (setq ,flags (logior ,flags DB_INIT_MPOOL)))))
      ,@(when init-rep `((when ,init-rep (setq ,flags (logior ,flags DB_INIT_REP)))))
      ,@(when init-txn `((when ,init-txn (setq ,flags (logior ,flags DB_INIT_TXN)))))
      ,@(when recover `((when ,recover (setq ,flags (logior ,flags DB_RECOVER)))))
      ,@(when recover-fatal `((when ,recover-fatal (setq ,flags (logior ,flags DB_RECOVER_FATAL)))))
      ,@(when lockdown `((when ,lockdown (setq ,flags (logior ,flags DB_LOCKDOWN)))))
      ,@(when private `((when ,private (setq ,flags (logior ,flags DB_PRIVATE)))))
      ,@(when system-mem `((when ,system-mem (setq ,flags (logior ,flags DB_SYSTEM_MEM)))))
      ,@(when thread `((when ,thread (setq ,flags (logior ,flags DB_THREAD)))))
      ,@(when force `((when ,force (setq ,flags (logior ,flags DB_FORCE)))))
      ,@(when degree-2 `((when ,degree-2 (setq ,flags (logior ,flags DB_DEGREE_2)))))
      ,@(when read-committed `((when ,read-committed (setq ,flags (logior ,flags DB_READ_COMMITTED)))))
      ,@(when dirty-read `((when ,dirty-read (setq ,flags (logior ,flags DB_DIRTY_READ)))))
      ,@(when read-uncommitted `((when ,read-uncommitted (setq ,flags (logior ,flags DB_READ_UNCOMMITTED)))))
      ,@(when create `((when ,create (setq ,flags (logior ,flags DB_CREATE)))))
      ,@(when excl `((when ,excl (setq ,flags (logior ,flags DB_EXCL)))))
      ,@(when nommap `((when ,nommap (setq ,flags (logior ,flags DB_NOMMAP)))))
      ,@(when rdonly `((when ,rdonly (setq ,flags (logior ,flags DB_RDONLY)))))
      ,@(when truncate `((when ,truncate (setq ,flags (logior ,flags DB_TRUNCATE)))))
      ,@(when txn-nosync `((when ,txn-nosync (setq ,flags (logior ,flags DB_TXN_NOSYNC)))))
      ,@(when txn-nowait `((when ,txn-nowait (setq ,flags (logior ,flags DB_TXN_NOWAIT)))))
      ,@(when txn-sync `((when ,txn-sync (setq ,flags (logior ,flags DB_TXN_SYNC)))))
      ,@(when freelist-only `((when ,freelist-only (setq ,flags (logior ,flags DB_FREELIST_ONLY)))))
      ,@(when free-space `((when ,free-space (setq ,flags (logior ,flags DB_FREE_SPACE)))))
      ,@(when lock-nowait `((when ,lock-nowait (setq ,flags (logior ,flags DB_LOCK_NOWAIT)))))
      ,@(when dup `((when ,dup (setq ,flags (logior ,flags DB_DUP)))))
      ,@(when dup-sort `((when ,dup-sort (setq ,flags (logior ,flags DB_DUPSORT)))))
      ,@(when current `((when ,current (setq ,flags (logior ,flags DB_CURRENT)))))
      ,@(when first `((when ,first (setq ,flags (logior ,flags DB_FIRST)))))
      ,@(when get-both `((when ,get-both (setq ,flags (logior ,flags DB_GET_BOTH)))))
      ,@(when get-both-range `((when ,get-both-range (setq ,flags (logior ,flags DB_GET_BOTH_RANGE)))))
      ,@(when last `((when ,last (setq ,flags (logior ,flags DB_LAST)))))
      ,@(when next `((when ,next (setq ,flags (logior ,flags DB_NEXT)))))
      ,@(when next-dup `((when ,next-dup (setq ,flags (logior ,flags DB_NEXT_DUP)))))
      ,@(when next-nodup `((when ,next-nodup (setq ,flags (logior ,flags DB_NEXT_NODUP)))))
      ,@(when prev `((when ,prev (setq ,flags (logior ,flags DB_PREV)))))
      ,@(when prev-nodup `((when ,prev-nodup (setq ,flags (logior ,flags DB_PREV_NODUP)))))
      ,@(when set `((when ,set (setq ,flags (logior ,flags DB_SET)))))
      ,@(when set-range `((when ,set-range (setq ,flags (logior ,flags DB_SET_RANGE)))))
      ,@(when after `((when ,after (setq ,flags (logior ,flags DB_AFTER)))))
      ,@(when before `((when ,before (setq ,flags (logior ,flags DB_BEFORE)))))
      ,@(when keyfirst `((when ,keyfirst (setq ,flags (logior ,flags DB_KEYFIRST)))))
      ,@(when keylast `((when ,keylast (setq ,flags (logior ,flags DB_KEYLAST)))))
      ,@(when no-dup-data `((when ,no-dup-data (setq ,flags (logior ,flags DB_NODUPDATA)))))
      ,@(when no-overwrite `((when ,no-overwrite (setq ,flags (logior ,flags DB_NOOVERWRITE)))))
      ,@(when nosync `((when ,nosync (setq ,flags (logior ,flags DB_NOSYNC)))))
      ,@(when position `((when ,position (setq ,flags (logior ,flags DB_POSITION)))))    
      ,@(when seq-dec `((when ,seq-dec (setq ,flags (logior ,flags DB_SEQ_DEC)))))
      ,@(when seq-inc `((when ,seq-inc (setq ,flags (logior ,flags DB_SEQ_INC)))))
      ,@(when seq-wrap `((when ,seq-wrap (setq ,flags (logior ,flags DB_SEQ_WRAP)))))
      ,@(when set-lock-timeout `((when ,set-lock-timeout (setq ,flags (logior ,flags DB_SET_LOCK_TIMEOUT)))))
      ,@(when set-transaction-timeout `((when ,set-transaction-timeout (setq ,flags (logior ,flags DB_SET_TXN_TIMEOUT)))))
      ,flags)))

;; Environment

(def-function ("db_env_cr" %db-env-create)
    ((flags :unsigned-int)
     (errno :int :out))
  :returning :pointer-void)

(defun db-env-create ()
  "Create an environment handle."
  (multiple-value-bind (env errno)
      (%db-env-create 0)
    (declare (type fixnum errno))
    (if (= errno 0)
	env
	(error 'db-error :errno errno))))
	     
(def-function ("db_env_close" %db-env-close)
    ((dbenvp :pointer-void)
     (flags :unsigned-int))
  :returning :int)

(wrap-errno db-env-close (dbenvp flags) 
	    :documentation "Close an environment handle.")

(def-function ("db_env_open" %db-env-open)
    ((dbenvp :pointer-void)
     (home :cstring)
     (flags :unsigned-int)
     (mode :int))
  :returning :int)

(wrap-errno db-env-open (dbenvp home flags mode)
	    :flags (auto-commit init-cdb init-lock init-log 
		    init-mpool init-rep init-txn
		    recover recover-fatal create
		    lockdown private system-mem thread
		    )
	    :keys ((mode #o640))
	    :cstrings (home)
	    :documentation "Open an environment handle.")

(def-function ("db_env_dbremove" %db-env-dbremove)
    ((env :pointer-void)
     (txn :pointer-void)
     (file :cstring)
     (database :cstring)
     (flags :unsigned-int))
  :returning :int)

(wrap-errno db-env-dbremove (env transaction file database flags) 
	    :flags (auto-commit)
	    :keys ((transaction (txn-default *current-transaction*))
		   (database +NULL-CHAR+))
	    :cstrings (file database)
	    :transaction transaction
	    :documentation "Remove a database.")

(def-function ("db_env_dbrename" %db-env-dbrename)
    ((env :pointer-void)
     (txn :pointer-void)
     (file :cstring)
     (database :cstring)
     (newname :cstring)
     (flags :unsigned-int))
  :returning :int)

(wrap-errno db-env-dbrename (env transaction file database newname flags) 
	    :flags (auto-commit)
	    :keys ((transaction (txn-default *current-transaction*))
		   (database +NULL-CHAR+))
	    :cstrings (file database newname)
	    :transaction transaction
	    :documentation "Rename an environment.")

(def-function ("db_env_remove" %db-env-remove)
    ((env :pointer-void)
     (home :cstring)
     (flags :unsigned-int))
  :returning :int)

(wrap-errno db-env-remove (env home flags) :flags (force)
	    :cstrings (home)
	    :documentation "Remove an environment.  Can't be called on an open handle.")

(def-function ("db_env_set_flags" %db-env-set-flags)
    ((env :pointer-void)
     (flags :unsigned-int)
     (onoff :int))
  :returning :int)

(wrap-errno db-env-set-flags (env flags onoff)
	    :flags (auto-commit nommap txn-nosync)
	    :documentation "Set flags on an environment.")

(def-function ("db_env_get_flags" %db-env-get-flags)
    ((env :pointer-void)
     (flags :unsigned-int :out))
  :returning :int)

(wrap-errno db-env-get-flags (env) :outs 2
	    :documentation "Get flags of an environment.")

(def-function ("db_env_txn_checkpoint" %db-env-txn-checkpoint)
    ((env :pointer-void)
     (kbyte :unsigned-int)
     (min :unsigned-int)
     (flags :unsigned-int))
  :returning :int)

(wrap-errno db-env-txn-checkpoint (env kbyte min flags)
	    :flags (force)
	    :documentation "Make a checkpoint.")

;;(def-function ("db_set_error_file" %db-set-error-file)
;;    ((db :pointer-void)
;;     (file :cstring)))

;;(defun db-set-error-file (db filename)
;;  (with-cstrings ((fname filename))
;;    (%db-set-error-file db fname)))

;; Database

(eval-when (:compile-toplevel :load-toplevel)
  (def-enum DBTYPE ((:BTREE 1) :HASH :QUEUE :RECNO :UNKNOWN)))

(def-function ("db_cr" %db-create)
    ((dbenv :pointer-void)
     (flags :unsigned-int)
     (errno :int :out))
  :returning :pointer-void)
	  
(defun db-create (&optional (dbenv +NULL-VOID+))
  "Create a DB handle."
  (multiple-value-bind (db errno)
      (%db-create dbenv 0)
    (declare (type fixnum errno))
    (if (= errno 0) 
	db
	(error 'db-error :errno errno))))

(def-function ("db_close" %db-close)
    ((db :pointer-void)
     (flags :unsigned-int))
  :returning :int)

(wrap-errno db-close (db flags)
	    :documentation "Close a DB handle.")

(def-function ("db_open" %db-open)
    ((db :pointer-void)
     (txn :pointer-void)
     (file :cstring)
     (database :cstring)
     (type DBTYPE)
     (flags :unsigned-int)
     (mode :int))
  :returning :int)

(wrap-errno db-open (db transaction file database type flags mode)
	    :flags (auto-commit create dirty-read read-uncommitted 
				excl nommap rdonly thread truncate
				)
	    :keys ((transaction (txn-default *current-transaction*))
		   (file +NULL-CHAR+)
		   (database +NULL-CHAR+)
		   (type DB-UNKNOWN)
		   (mode #o640))
	    :cstrings (file database)
	    :transaction transaction
	    :documentation "Open a DB handle.  If you want transactions, be sure to open the handle with a transaction (or auto-commit.)")
		
(def-function ("db_remove" %db-remove)
    ((db :pointer-void)
     (file :cstring)
     (database :cstring)
     (flags :unsigned-int))
  :returning :int)

(wrap-errno db-remove (db file database flags)
	    :keys ((database +NULL-CHAR+))
	    :cstrings (file database)
	    :documentation "Remove a DB handle.")

(def-function ("db_rename" %db-rename)
    ((db :pointer-void)
     (file :cstring)
     (database :cstring)
     (newname :cstring)
     (flags :unsigned-int))
  :returning :int)

(wrap-errno db-rename (db file database newname flags)
	    :keys ((database +NULL-CHAR+))
	    :cstrings (file database newname)
	    :documentation "Rename a DB handle.")

(def-function ("db_sync" %db-sync)
    ((db :pointer-void)
     (flags :unsigned-int))
  :returning :int)

(wrap-errno db-sync (db flags) :documentation "Sync a DB.")

(def-function ("db_truncate" %db-truncate)
    ((db :pointer-void)
     (txn :pointer-void)
     (count :unsigned-int :out)
     (flags :unsigned-int))
  :returning :int)

(wrap-errno db-truncate (db transaction flags) :flags (auto-commit) 
	    :keys ((transaction (txn-default *current-transaction*))) 
	    :outs 2
	    :transaction transaction
	    :documentation "Truncate (erase) a DB.")

(def-function ("db_set_flags" %db-set-flags)
    ((db :pointer-void)
     (flags :unsigned-int))
  :returning :int)

(wrap-errno db-set-flags (db flags)
	    :flags (dup dup-sort)
	    :documentation 
"Sets flags on a DB handle.  Currently this means only DUP
and DUP-SORT.")

(def-function ("db_get_flags" %db-get-flags)
    ((db :pointer-void)
     (flags :unsigned-int :out))
  :returning :int)

(defun db-get-flags (db)
  "Get flags on a DB handle."
  (multiple-value-bind (errno flags)
      (%db-get-flags db)
    (if (= errno 0) flags
	(error 'db-error :errno errno))))

;; Accessors

(def-function ("db_get_raw" %db-get-key-buffered)
    ((db :pointer-void)
     (txn :pointer-void)
     (key array-or-pointer-char)
     (key-size :unsigned-int)
     (buffer array-or-pointer-char)
     (buffer-length :unsigned-int)
     (flags :unsigned-int)
     (result-size :unsigned-int :out))
  :returning :int)

(defun db-get-key-buffered (db key-buffer-stream value-buffer-stream
			    &key (transaction (txn-default *current-transaction*))
			    get-both degree-2 read-committed
			    dirty-read read-uncommitted)
  "Get a key / value pair from a DB.  The key is encoded in
a buffer-stream.  Space for the value is passed in as a
buffer-stream.  On success the buffer-stream is returned for
decoding, or NIL if nothing was found."
  (declare (type pointer-void db transaction)
	   (type buffer-stream key-buffer-stream value-buffer-stream)
	   (type boolean get-both degree-2 read-committed dirty-read read-uncommitted))
  (loop 
   for value-length fixnum = (buffer-stream-length value-buffer-stream)
   do
   (multiple-value-bind (errno result-size)
       (%db-get-key-buffered db transaction 
			     (buffer-stream-buffer key-buffer-stream)
			     (buffer-stream-size key-buffer-stream)
			     (buffer-stream-buffer value-buffer-stream) 
			     value-length
			     (flags :get-both get-both
				    :degree-2 (or degree-2 read-committed)
				    :dirty-read (or dirty-read read-uncommitted)))
     (declare (type fixnum result-size errno))
     (cond 
       ((= errno 0)
	(setf (buffer-stream-size value-buffer-stream) result-size)
	(return-from db-get-key-buffered 
	  (the buffer-stream value-buffer-stream)))
       ((or (= errno DB_NOTFOUND) (= errno DB_KEYEMPTY))
	(return-from db-get-key-buffered nil))
       ((or (= errno DB_LOCK_DEADLOCK) (= errno DB_LOCK_NOTGRANTED))
	(throw 'transaction transaction))
       ((> result-size value-length)
	(resize-buffer-stream-no-copy value-buffer-stream result-size))
       (t (error 'db-error :errno errno))))))

(def-function ("db_get_raw" %db-get-buffered)
    ((db :pointer-void)
     (txn :pointer-void)
     (key :cstring)
     (key-size :unsigned-int)
     (buffer array-or-pointer-char)
     (buffer-length :unsigned-int)
     (flags :unsigned-int)
     (result-size :unsigned-int :out))
  :returning :int)

(defun db-get-buffered (db key value-buffer-stream &key
			(key-size (length key))
			(transaction (txn-default *current-transaction*))
			get-both degree-2 read-committed
			dirty-read read-uncommitted)
  "Get a key / value pair from a DB.  The key is passed as a
string.  Space for the value is passed in as a
buffer-stream.  On success the buffer-stream is returned for
decoding, or NIL if nothing was found."
  (declare (type pointer-void db transaction)
	   (type string key)
	   (type buffer-stream value-buffer-stream)
	   (type fixnum key-size)
	   (type boolean get-both degree-2 read-committed 
		 dirty-read read-uncommitted))
  (with-cstring (k key)
    (loop 
     for value-length fixnum = (buffer-stream-length value-buffer-stream)
     do
     (multiple-value-bind (errno result-size)
	 (%db-get-buffered db transaction k key-size 
			   (buffer-stream-buffer value-buffer-stream)
			   value-length
			   (flags :get-both get-both
				  :degree-2 (or degree-2 read-committed)
				  :dirty-read (or dirty-read read-uncommitted)))
       (declare (type fixnum result-size errno))
       (cond 
	 ((= errno 0)
	  (setf (buffer-stream-size value-buffer-stream) result-size)
	  (return-from db-get-buffered 
	    (the buffer-stream value-buffer-stream)))
	 ((or (= errno DB_NOTFOUND) (= errno DB_KEYEMPTY))
	  (return-from db-get-buffered nil))
	 ((or (= errno DB_LOCK_DEADLOCK) (= errno DB_LOCK_NOTGRANTED))
	  (throw 'transaction transaction))
	 ((> result-size value-length)
	  (resize-buffer-stream-no-copy value-buffer-stream result-size))
	 (t (error 'db-error :errno errno)))))))

(defun db-get (db key &key (key-size (length key))
	       (transaction (txn-default *current-transaction*))
	       get-both degree-2 read-committed
	       dirty-read read-uncommitted)
  "Get a key / value pair from a DB.  The key is passed as a
string, and the value is returned as a string.  If nothing
is found, NIL is returned."
  (declare (type pointer-void db transaction)
	   (type string key)
	   (type fixnum key-size)
	   (type boolean get-both degree-2 read-committed
		 dirty-read read-uncommitted))
  (with-cstring (k key)
    (with-buffer-streams (value-buffer-stream)
      (loop 
       for value-length fixnum = (buffer-stream-length value-buffer-stream)
       do
       (multiple-value-bind (errno result-size)
	   (%db-get-buffered db transaction k key-size 
			     (buffer-stream-buffer value-buffer-stream)
			     value-length
			     (flags :get-both get-both
				    :degree-2 (or degree-2 read-committed)
				    :dirty-read (or dirty-read read-uncommitted)))
	 (declare (type fixnum result-size errno))
	 (cond
	   ((= errno 0)
	    (return-from db-get
	      (convert-from-foreign-string (buffer-stream-buffer
					    value-buffer-stream)
					   :length result-size
					   :null-terminated-p nil)))
	   ((or (= errno DB_NOTFOUND) (= errno DB_KEYEMPTY))
	    (return-from db-get nil))
	   ((or (= errno DB_LOCK_DEADLOCK) (= errno DB_LOCK_NOTGRANTED))
	    (throw 'transaction transaction))
	   ((> result-size value-length)
	    (resize-buffer-stream-no-copy value-buffer-stream result-size))
	   (t (error 'db-error :errno errno))))))))

(def-function ("db_put_raw" %db-put-buffered)
    ((db :pointer-void)
     (txn :pointer-void)
     (key array-or-pointer-char)
     (key-size :unsigned-int)
     (value array-or-pointer-char)
     (value-size :unsigned-int)
     (flags :unsigned-int))
  :returning :int)

(defun db-put-buffered (db key-buffer-stream value-buffer-stream
			&key (transaction (txn-default *current-transaction*))
			exists-error-p)
  "Put a key / value pair into a DB.  The pair are encoded
in buffer-streams.  T on success, or nil if the key already
exists and EXISTS-ERROR-P is NIL."
  (declare (type pointer-void db transaction)
	   (type buffer-stream key-buffer-stream value-buffer-stream)
	   (type boolean exists-error-p))
  (let ((errno 
	 (%db-put-buffered db transaction 
			   (buffer-stream-buffer key-buffer-stream)
			   (buffer-stream-size key-buffer-stream)
			   (buffer-stream-buffer value-buffer-stream)
			   (buffer-stream-size value-buffer-stream)
			   0)))
    (declare (type fixnum errno))
    (cond ((= errno 0) t)
	  ((and (= errno DB_KEYEXIST) (not exists-error-p))
	   nil)
	  ((or (= errno DB_LOCK_DEADLOCK) (= errno DB_LOCK_NOTGRANTED))
	   (throw 'transaction transaction))
	  (t (error 'db-error :errno errno)))))

(def-function ("db_put_raw" %db-put)
    ((db :pointer-void)
     (txn :pointer-void)
     (key :cstring)
     (key-size :unsigned-int)
     (value :cstring)
     (value-size :unsigned-int)
     (flags :unsigned-int))
  :returning :int)

(wrap-errno db-put (db transaction key key-size value value-size flags)
	    :flags ()
	    :keys ((key-size (length key))
		   (value-size (length value))
		   (transaction (txn-default *current-transaction*)))
	    :cstrings (key value)
	    :declarations (declare (type pointer-void db transaction)
				   (type string key value)
				   (type fixnum key-size value-size))
	    :transaction transaction
	    :documentation   
"Put a key / value pair into a DB.  The pair are strings.")

(def-function ("db_del" %db-delete-buffered)
    ((db :pointer-void)
     (txn :pointer-void)
     (key array-or-pointer-char)
     (key-size :unsigned-int)
     (flags :unsigned-int))
  :returning :int)

(defun db-delete-buffered  (db key-buffer-stream 
			    &key (transaction (txn-default *current-transaction*)))
  "Delete a key / value pair from a DB.  The key is encoded
in a buffer-stream.  T on success, NIL if the key wasn't
found."
  (declare (type pointer-void db transaction) 
	   (type buffer-stream key-buffer-stream))
  (let ((errno (%db-delete-buffered db transaction
				    (buffer-stream-buffer key-buffer-stream)
				    (buffer-stream-size key-buffer-stream)
				    0)))
    (declare (type fixnum errno))
    (cond ((= errno 0) t)
	  ((or (= errno DB_NOTFOUND) 
	       (= errno DB_KEYEMPTY))
	   nil)
	  ((or (= errno DB_LOCK_DEADLOCK)
	       (= errno DB_LOCK_NOTGRANTED))
	   (throw 'transaction transaction))
	  (t (error 'db-error :errno errno)))))

(def-function ("db_del" %db-delete)
    ((db :pointer-void)
     (txn :pointer-void)
     (key :cstring)
     (key-size :unsigned-int)
     (flags :unsigned-int))
  :returning :int)

(defun db-delete (db key &key (key-size (length key))
		  (transaction (txn-default *current-transaction*)))
  "Delete a key / value pair from a DB.  The key is a
string.  T on success, NIL if the key wasn't found."
  (declare (type pointer-void db transaction) (type string key)
	   (type fixnum key-size))
  (with-cstrings ((key key))
    (let ((errno
	   (%db-delete db transaction key
		       key-size 0)))
      (declare (type fixnum errno))
      (cond ((= errno 0) t)
	    ((or (= errno DB_NOTFOUND) 
		 (= errno DB_KEYEMPTY))
	     nil)
	    ((or (= errno DB_LOCK_DEADLOCK)
		 (= errno DB_LOCK_NOTGRANTED))
	     (throw 'transaction transaction))
	    (t (error 'db-error :errno errno))))))

(def-function ("db_del_kv" %db-delete-kv)
    ((db :pointer-void)
     (txn :pointer-void)
     (key array-or-pointer-char)
     (key-size :unsigned-int)
     (value array-or-pointer-char)
     (value-size :unsigned-int))
  :returning :int)

(defun db-delete-kv-buffered  (db key-buffer-stream value-buffer-stream
			       &key (transaction (txn-default *current-transaction*)))
  "Delete a specific key / value pair from a DB with
duplicates.  The key and value are encoded as
buffer-streams.  T on success, NIL if the key / value pair
wasn't found."
  (declare (type pointer-void db transaction) 
	   (type buffer-stream key-buffer-stream value-buffer-stream))
  (let ((errno (%db-delete-kv db transaction
			      (buffer-stream-buffer key-buffer-stream)
			      (buffer-stream-size key-buffer-stream)
			      (buffer-stream-buffer value-buffer-stream)
			      (buffer-stream-size value-buffer-stream))))
    (declare (type fixnum errno))
    (cond ((= errno 0) t)
	  ((or (= errno DB_NOTFOUND) 
	       (= errno DB_KEYEMPTY))
	   nil)
	  ((or (= errno DB_LOCK_DEADLOCK)
	       (= errno DB_LOCK_NOTGRANTED))
	   (throw 'transaction transaction))
	  (t (error 'db-error :errno errno)))))

;; Compaction for BDB 4.4

(def-function ("db_compact" %db-compact)
    ((db :pointer-void)
     (txn :pointer-void)
     (start array-or-pointer-char)
     (start-size :unsigned-int)
     (stop array-or-pointer-char)
     (stop-size :unsigned-int)
     (flags :unsigned-int)
     (end array-or-pointer-char)
     (end-length :unsigned-int)
     (end-size :unsigned-int :out))
  :returning :int)

(defun db-compact (db start stop end &key (transaction (txn-default *current-transaction*))
		   freelist-only free-space)
  (declare (type pointer-void db transaction)
	   (type buffer-stream start stop)
	   (type boolean freelist-only free-space))
  (loop
       for end-length fixnum = (buffer-stream-length end)
       do
	 (multiple-value-bind (errno end-size)
	     (%db-compact db transaction 
			  (if start (buffer-stream-buffer start) 0)
			  (if start (buffer-stream-size start) 0)
			  (if stop (buffer-stream-buffer stop) 0)
			  (if stop (buffer-stream-size stop) 0)
			  (flags :freelist-only freelist-only :free-space free-space)
			  (buffer-stream-buffer end)
			  (buffer-stream-length end))
	   (declare (type fixnum errno end-size))
	   (cond ((= errno 0)
		  (setf (buffer-stream-size end) end-size)
		  (return-from db-compact (the buffer-stream end)))
		 ((or (= errno DB_NOTFOUND) (= errno DB_KEYEMPTY))
		  (return-from db-compact nil))
		 ((or (= errno DB_LOCK_DEADLOCK) (= errno DB_LOCK_NOTGRANTED))
		  (throw 'transaction transaction))
		 ((> end-size end-length)
		  (resize-buffer-stream-no-copy end end-size))
		 (t (error 'db-error :errno errno))))))

;; Cursors

(def-function ("db_cursor" %db-cursor)
    ((db :pointer-void)
     (txn :pointer-void)
     (flags :unsigned-int)
     (errnop (* :int)))
  :returning :pointer-void)

(defun db-cursor (db &key (transaction (txn-default *current-transaction*))
		  degree-2 read-committed dirty-read read-uncommitted)
  "Create a cursor."
  (declare (type pointer-void db)
	   (type boolean degree-2 read-committed dirty-read read-uncommitted))
  (let ((errno-buffer (allocate-foreign-object :int 1)))
    (declare (type pointer-int errno-buffer))
    (let* ((curs (%db-cursor db transaction 
			     (flags :degree-2 (or degree-2 read-committed)
				    :dirty-read (or dirty-read read-uncommitted))
			     errno-buffer))
	   (errno (deref-array errno-buffer '(:array :int) 0)))
      (declare (type pointer-void curs)
	       (type fixnum errno))
      (if (= errno 0) curs
	  (error 'db-error :errno errno)))))

(def-function ("db_cursor_close" %db-cursor-close)
    ((cursor :pointer-void))
  :returning :int)

(wrap-errno db-cursor-close (cursor) :documentation "Close a cursor.")

(def-function ("db_cursor_del" %db-cursor-delete)
    ((cursor :pointer-void)
     (flags :unsigned-int))
  :returning :int)

(defun db-cursor-delete (cursor)
  "Delete by cursor."
  (declare (type pointer-void cursor))
  (let ((errno (%db-cursor-delete cursor 0)))
    (declare (type fixnum errno))
    (cond ((= errno 0) t)
	  ((or (= errno DB_NOTFOUND)
	       (= errno DB_KEYEMPTY))
	   nil)
	  ((or (= errno DB_LOCK_DEADLOCK)
	       (= errno DB_LOCK_NOTGRANTED))
	   (throw 'transaction *current-transaction*))
	  (t (error 'db-error :errno errno)))))

(def-function ("db_cursor_dup" %db-cursor-dup)
    ((cursor :pointer-void)
     (flags :unsigned-int)
     (errnop (* :int)))
  :returning :pointer-void)

(defun db-cursor-duplicate (cursor &key (position t)) 
  "Duplicate a cursor."
  (declare (type pointer-void cursor))
  (let ((errno-buffer (allocate-foreign-object :int 1)))
    (declare (type pointer-int errno-buffer))
    (let* ((newc (%db-cursor-dup cursor (flags :position position) 
				 errno-buffer))
	   (errno (deref-array errno-buffer '(:array :int) 0)))
      (declare (type pointer-void newc)
	       (type fixnum errno))
      (if (= errno 0) newc
	  (error 'db-error :errno errno)))))

(def-function ("db_cursor_get_raw" %db-cursor-get-key-buffered)
    ((cursor :pointer-void)
     (key array-or-pointer-char)
     (key-size :unsigned-int)
     (key-length :unsigned-int)
     (buffer array-or-pointer-char)
     (buffer-size :unsigned-int)
     (buffer-length :unsigned-int)
     (flags :unsigned-int)
     (ret-key-size :unsigned-int :out)
     (result-size :unsigned-int :out))
  :returning :int)

;; current, first, last, next, next-dup, next-nodup, prev,
;; prev-nodup : sets nothing
(defun db-cursor-move-buffered (cursor key-buffer-stream value-buffer-stream
				&key current first last next next-dup 
				next-nodup prev prev-nodup dirty-read read-uncommitted)
  "Move a cursor, returning the key / value pair found.
Supports current, first, last, next, next-dup, next-nodup,
prev, prev-nodup."
  (declare (type pointer-void cursor)
	   (type buffer-stream key-buffer-stream value-buffer-stream)
	   (type boolean current first last next next-dup next-nodup prev 
		 prev-nodup dirty-read read-uncommitted))
  (loop 
   for key-length fixnum = (buffer-stream-length key-buffer-stream)
   for value-length fixnum = (buffer-stream-length value-buffer-stream)
   do
   (multiple-value-bind (errno ret-key-size result-size)
       (%db-cursor-get-key-buffered cursor 
				    (buffer-stream-buffer key-buffer-stream)
				    0 key-length
				    (buffer-stream-buffer value-buffer-stream)
				    0 value-length
				    (flags :current current
					   :first first
					   :last last
					   :next next
					   :next-dup next-dup
					   :next-nodup next-nodup
					   :prev prev
					   :prev-nodup prev-nodup
					   :dirty-read (or dirty-read read-uncommitted)))
     (declare (type fixnum errno ret-key-size result-size))
     (cond 
       ((= errno 0)
	(setf (buffer-stream-size key-buffer-stream) ret-key-size)
	(setf (buffer-stream-size value-buffer-stream) result-size)
	(return-from db-cursor-move-buffered 
	  (the (values buffer-stream buffer-stream)
	    (values key-buffer-stream value-buffer-stream))))
       ((or (= errno DB_NOTFOUND) (= errno DB_KEYEMPTY))
	(return-from db-cursor-move-buffered (values nil nil)))
       ((or (= errno DB_LOCK_DEADLOCK) (= errno DB_LOCK_NOTGRANTED))
	(throw 'transaction *current-transaction*))
       ((or (> result-size value-length) (> ret-key-size key-length))
	(resize-buffer-stream-no-copy value-buffer-stream result-size)
	(resize-buffer-stream-no-copy key-buffer-stream ret-key-size))
       (t (error 'db-error :errno errno))))))

;; set, set-range: sets key
(defun db-cursor-set-buffered (cursor key-buffer-stream value-buffer-stream
			       &key set set-range dirty-read read-uncommitted)
  "Move a cursor to a key, returning the key / value pair
found.  Supports set and set-range."
  (declare (type pointer-void cursor)
	   (type buffer-stream key-buffer-stream value-buffer-stream)
	   (type boolean set set-range dirty-read read-uncommitted))
  (loop 
   for key-length fixnum = (buffer-stream-length key-buffer-stream)
   for value-length fixnum = (buffer-stream-length value-buffer-stream)
   do
   (multiple-value-bind (errno ret-key-size result-size)
       (%db-cursor-get-key-buffered cursor 
				    (buffer-stream-buffer key-buffer-stream)
				    (buffer-stream-size key-buffer-stream)
				    key-length
				    (buffer-stream-buffer value-buffer-stream)
				    0 value-length
				    (flags :set set
					   :set-range set-range
					   :dirty-read (or dirty-read read-uncommitted)))
     (declare (type fixnum errno ret-key-size result-size))
     (cond 
       ((= errno 0)
	(setf (buffer-stream-size key-buffer-stream) ret-key-size)
	(setf (buffer-stream-size value-buffer-stream) result-size)
	(return-from db-cursor-set-buffered 
	  (the (values buffer-stream buffer-stream)
	    (values key-buffer-stream value-buffer-stream))))
       ((or (= errno DB_NOTFOUND) (= errno DB_KEYEMPTY))
	(return-from db-cursor-set-buffered (values nil nil)))
       ((or (= errno DB_LOCK_DEADLOCK) (= errno DB_LOCK_NOTGRANTED))
	(throw 'transaction *current-transaction*))
       ((or (> result-size value-length) (> ret-key-size key-length))
	(resize-buffer-stream-no-copy value-buffer-stream result-size)
	(resize-buffer-stream key-buffer-stream ret-key-size))
       (t (error 'db-error :errno errno))))))

;; get-both, get-both-range : sets both
(defun db-cursor-get-both-buffered (cursor key-buffer-stream 
				    value-buffer-stream
				    &key get-both get-both-range dirty-read read-uncommitted)
  "Move a cursor to a key / value pair, returning the key /
value pair found.  Supports get-both and get-both-range."
  (declare (type pointer-void cursor)
	   (type buffer-stream key-buffer-stream value-buffer-stream)
	   (type boolean get-both get-both-range dirty-read read-uncommitted))
  (loop 
   for key-length fixnum = (buffer-stream-length key-buffer-stream)
   for value-length fixnum = (buffer-stream-length value-buffer-stream)
   do
   (multiple-value-bind (errno ret-key-size result-size)
       (%db-cursor-get-key-buffered cursor 
				    (buffer-stream-buffer key-buffer-stream)
				    (buffer-stream-size	key-buffer-stream)
				    key-length
				    (buffer-stream-buffer value-buffer-stream)
				    (buffer-stream-size	value-buffer-stream)
				    value-length
				    (flags :get-both get-both
					   :get-both-range get-both-range
					   :dirty-read (or dirty-read read-uncommitted)))
     (declare (type fixnum errno ret-key-size result-size))
     (cond 
       ((= errno 0)
	(setf (buffer-stream-size key-buffer-stream) ret-key-size)
	(setf (buffer-stream-size value-buffer-stream) result-size)
	(return-from db-cursor-get-both-buffered 
	  (the (values buffer-stream buffer-stream)
	    (values key-buffer-stream value-buffer-stream))))
       ((or (= errno DB_NOTFOUND) (= errno DB_KEYEMPTY))
	(return-from db-cursor-get-both-buffered (values nil nil)))
       ((or (= errno DB_LOCK_DEADLOCK) (= errno DB_LOCK_NOTGRANTED))
	(throw 'transaction *current-transaction*))
       ((or (> result-size value-length) (> ret-key-size key-length))
	(resize-buffer-stream key-buffer-stream ret-key-size)
	(resize-buffer-stream value-buffer-stream result-size))
       (t (error 'db-error :errno errno))))))

(def-function ("db_cursor_pget_raw" %db-cursor-pget-key-buffered)
    ((cursor :pointer-void)
     (key array-or-pointer-char)
     (key-size :unsigned-int)
     (key-length :unsigned-int)
     (pkey array-or-pointer-char)
     (pkey-size :unsigned-int)
     (pkey-length :unsigned-int)
     (buffer array-or-pointer-char)
     (buffer-size :unsigned-int)
     (buffer-length :unsigned-int)
     (flags :unsigned-int)
     (ret-key-size :unsigned-int :out)
     (ret-pkey-size :unsigned-int :out)
     (result-size :unsigned-int :out))
  :returning :int)

;; current, first, last, next, next-dup, next-nodup, prev,
;; prev-nodup : sets nothing
(defun db-cursor-pmove-buffered (cursor key-buffer-stream pkey-buffer-stream 
				 value-buffer-stream
				 &key current first last next next-dup 
				 next-nodup prev prev-nodup dirty-read)
  "Move a secondary cursor, returning the key / value /
primary triple found.  Supports current, first, last, next,
next-dup, next-nodup, prev, prev-nodup."
  (declare (type pointer-void cursor)
	   (type buffer-stream key-buffer-stream pkey-buffer-stream 
		 value-buffer-stream)
	   (type boolean current first last next next-dup next-nodup prev 
		 prev-nodup dirty-read))
  (loop 
   for key-length fixnum = (buffer-stream-length key-buffer-stream)
   for pkey-length fixnum = (buffer-stream-length pkey-buffer-stream)
   for value-length fixnum = (buffer-stream-length value-buffer-stream)
   do
   (multiple-value-bind (errno ret-key-size ret-pkey-size result-size)
       (%db-cursor-pget-key-buffered cursor 
				     (buffer-stream-buffer key-buffer-stream)
				     0 key-length
				     (buffer-stream-buffer pkey-buffer-stream)
				     0 pkey-length
				     (buffer-stream-buffer value-buffer-stream)
				     0 value-length
				     (flags :current current
					    :first first
					    :last last
					    :next next
					    :next-dup next-dup
					    :next-nodup next-nodup
					    :prev prev
					    :prev-nodup prev-nodup
					    :dirty-read dirty-read))
     (declare (type fixnum errno ret-key-size ret-pkey-size result-size))
     (cond 
       ((= errno 0)
	(setf (buffer-stream-size key-buffer-stream) ret-key-size)
	(setf (buffer-stream-size pkey-buffer-stream) ret-pkey-size)
	(setf (buffer-stream-size value-buffer-stream) result-size)
	(return-from db-cursor-pmove-buffered 
	  (the (values buffer-stream buffer-stream buffer-stream)
	    (values key-buffer-stream pkey-buffer-stream 
		    value-buffer-stream))))
       ((or (= errno DB_NOTFOUND) (= errno DB_KEYEMPTY))
	(return-from db-cursor-pmove-buffered (values nil nil nil)))
       ((or (= errno DB_LOCK_DEADLOCK) (= errno DB_LOCK_NOTGRANTED))
	(throw 'transaction *current-transaction*))
       ((or (> result-size value-length) 
	    (> ret-pkey-size pkey-length)
	    (> ret-key-size key-length))
	(resize-buffer-stream-no-copy value-buffer-stream result-size)
	(resize-buffer-stream-no-copy pkey-buffer-stream ret-pkey-size)
	(resize-buffer-stream-no-copy key-buffer-stream ret-key-size))
       (t (error 'db-error :errno errno))))))

;; set, set-range: sets key
(defun db-cursor-pset-buffered (cursor key-buffer-stream pkey-buffer-stream 
				value-buffer-stream
				&key set set-range dirty-read)
  "Move a secondary cursor tp a key, returning the key / value /
primary triple found.  Supports set, set-range."
  (declare (type pointer-void cursor)
	   (type buffer-stream key-buffer-stream pkey-buffer-stream 
		 value-buffer-stream)
	   (type boolean set set-range dirty-read))
  (loop 
   for key-length fixnum = (buffer-stream-length key-buffer-stream)
   for pkey-length fixnum = (buffer-stream-length pkey-buffer-stream)
   for value-length fixnum = (buffer-stream-length value-buffer-stream)
   do
   (multiple-value-bind (errno ret-key-size ret-pkey-size result-size)
       (%db-cursor-pget-key-buffered cursor 
				     (buffer-stream-buffer key-buffer-stream)
				     (buffer-stream-size key-buffer-stream)
				     key-length
				     (buffer-stream-buffer pkey-buffer-stream)
				     0 pkey-length
				     (buffer-stream-buffer value-buffer-stream)
				     0 value-length
				     (flags :set set
					    :set-range set-range
					    :dirty-read dirty-read))
     (declare (type fixnum errno ret-key-size ret-pkey-size result-size))
     (cond 
       ((= errno 0)
	(setf (buffer-stream-size key-buffer-stream) ret-key-size)
	(setf (buffer-stream-size pkey-buffer-stream) ret-pkey-size)
	(setf (buffer-stream-size value-buffer-stream) result-size)
	(return-from db-cursor-pset-buffered 
	  (the (values buffer-stream buffer-stream buffer-stream)
	    (values key-buffer-stream pkey-buffer-stream
		    value-buffer-stream))))
       ((or (= errno DB_NOTFOUND) (= errno DB_KEYEMPTY))
	(return-from db-cursor-pset-buffered (values nil nil nil)))
       ((or (= errno DB_LOCK_DEADLOCK) (= errno DB_LOCK_NOTGRANTED))
	(throw 'transaction *current-transaction*))
       ((or (> result-size value-length) 
	    (> ret-pkey-size pkey-length)
	    (> ret-key-size key-length))
	(resize-buffer-stream-no-copy value-buffer-stream result-size)
	(resize-buffer-stream-no-copy pkey-buffer-stream ret-pkey-size)
	(resize-buffer-stream key-buffer-stream ret-key-size))
       (t (error 'db-error :errno errno))))))

;; get-both, get-both-range : sets key and primary
(defun db-cursor-pget-both-buffered (cursor key-buffer-stream 
				     pkey-buffer-stream 
				     value-buffer-stream
				     &key get-both get-both-range dirty-read)
  "Move a secondary cursor tp a key / primary pair,
returning the key / value / primary triple found.  Supports
get, get-range."
  (declare (type pointer-void cursor)
	   (type buffer-stream key-buffer-stream pkey-buffer-stream 
		 value-buffer-stream)
	   (type boolean get-both get-both-range dirty-read))
  (loop 
   for key-length fixnum = (buffer-stream-length key-buffer-stream)
   for pkey-length fixnum = (buffer-stream-length pkey-buffer-stream)
   for value-length fixnum = (buffer-stream-length value-buffer-stream)
   do
   (multiple-value-bind (errno ret-key-size ret-pkey-size result-size)
       (%db-cursor-pget-key-buffered cursor 
				     (buffer-stream-buffer key-buffer-stream)
				     (buffer-stream-size key-buffer-stream)
				     key-length
				     (buffer-stream-buffer pkey-buffer-stream)
				     (buffer-stream-size pkey-buffer-stream)
				     pkey-length
				     (buffer-stream-buffer value-buffer-stream)
				     0 value-length
				     (flags :get-both get-both
					   :get-both-range get-both-range
					   :dirty-read dirty-read))
     (declare (type fixnum errno ret-key-size ret-pkey-size result-size))
     (cond 
       ((= errno 0)
	(setf (buffer-stream-size key-buffer-stream) ret-key-size)
	(setf (buffer-stream-size pkey-buffer-stream) ret-pkey-size)
	(setf (buffer-stream-size value-buffer-stream) result-size)
	(return-from db-cursor-pget-both-buffered 
	  (the (values buffer-stream buffer-stream buffer-stream)
	    (values key-buffer-stream pkey-buffer-stream 
		    value-buffer-stream))))
       ((or (= errno DB_NOTFOUND) (= errno DB_KEYEMPTY))
	(return-from db-cursor-pget-both-buffered (values nil nil nil)))
       ((or (= errno DB_LOCK_DEADLOCK) (= errno DB_LOCK_NOTGRANTED))
	(throw 'transaction *current-transaction*))
       ((or (> result-size value-length) 
	    (> ret-pkey-size pkey-length)
	    (> ret-key-size key-length))
	(resize-buffer-stream key-buffer-stream ret-key-size)
	(resize-buffer-stream pkey-buffer-stream ret-pkey-size)
	(resize-buffer-stream-no-copy value-buffer-stream result-size))
       (t (error 'db-error :errno errno))))))

(def-function ("db_cursor_put_raw" %db-cursor-put-buffered)
    ((cursor :pointer-void)
     (key array-or-pointer-char)
     (key-size :unsigned-int)
     (value array-or-pointer-char)
     (value-size :unsigned-int)
     (flags :unsigned-int))
  :returning :int)

(defun db-cursor-put-buffered (cursor key-buffer-stream value-buffer-stream
			       &key after before current keyfirst keylast
			       no-dup-data exists-error-p)
  "Put by cursor.  The key and value are encoded as buffer-streams."
  (declare (type pointer-void cursor)
	   (type buffer-stream key-buffer-stream value-buffer-stream)
	   (type boolean after before current keyfirst keylast no-dup-data
		 exists-error-p))
  (let ((errno (%db-cursor-put-buffered 
		cursor 
		(buffer-stream-buffer key-buffer-stream) 
		(buffer-stream-size key-buffer-stream)
		(buffer-stream-buffer value-buffer-stream) 
		(buffer-stream-size value-buffer-stream)
		(flags :after after
		       :before before
		       :current current
		       :keyfirst keyfirst
		       :keylast keylast
		       :no-dup-data no-dup-data))))
    (declare (type fixnum errno))
    (cond 
      ((= errno 0) t)
      ((and (= errno DB_KEYEXIST) (not exists-error-p))
       nil)
      ((or (= errno DB_LOCK_DEADLOCK) (= errno DB_LOCK_NOTGRANTED))
       (throw 'transaction *current-transaction*))
      (t (error 'db-error :errno errno)))))

;; Transactions

(def-function ("db_txn_begin" %db-txn-begin)
    ((env :pointer-void)
     (parent :pointer-void)
     (flags :unsigned-int)
     (errno (* :int)))
  :returning :pointer-void)

(defun db-transaction-begin (env &key parent
			     degree-2 read-committed dirty-read read-uncommitted
			     txn-nosync txn-nowait txn-sync)
  "Start a transaction.  Transactions may be nested."
  (declare (type pointer-void env parent)
	   (type boolean degree-2 read-committed dirty-read read-uncommitted 
		 txn-nosync txn-nowait txn-sync))
  (let ((errno-buffer (allocate-foreign-object :int 1)))
    (declare (type pointer-int errno-buffer))
    (let* ((txn
	    (%db-txn-begin env parent
			   (flags :degree-2 (or degree-2 read-committed)
				  :dirty-read (or dirty-read read-uncommitted)
				  :txn-nosync txn-nosync
				  :txn-nowait txn-nowait
				  :txn-sync txn-sync)
			   errno-buffer))
	   (errno (deref-array errno-buffer '(:array :int) 0)))
      (declare (type pointer-void txn)
	       (type fixnum errno))
      (if (= errno 0) 
	  txn
	  (error 'db-error :errno errno)))))

(def-function ("db_txn_abort" %db-txn-abort)
    ((txn :pointer-void))
  :returning :int)

(wrap-errno (db-transaction-abort %db-txn-abort) (transaction)
	    :declarations (declare (type pointer-void transaction))
	    :documentation "Abort a transaction.")

(def-function ("db_txn_commit" %db-txn-commit)
    ((txn :pointer-void)
     (flags :unsigned-int))
  :returning :int)

(wrap-errno (db-transaction-commit %db-txn-commit) (transaction flags)
	    :flags (txn-nosync txn-sync)
	    :declarations (declare (type pointer-void transaction)
				   (type boolean txn-nosync txn-sync))
	    :documentation "Commit a transaction.")

;; Locks and timeouts

(def-function ("db_txn_id" %db-transaction-id)
    ((transaction :pointer-void))
  :returning :unsigned-int)

(defun db-transaction-id (&optional (transaction (txn-default *current-transaction*)))
  "Returns the ID of the transaction (for locking purposes.)"
  (%db-transaction-id transaction))

(def-function ("db_env_lock_id" %db-env-lock-id)
    ((env :pointer-void)
     (id :unsigned-int :out))
  :returning :int)

(wrap-errno db-env-lock-id (env) :outs 2 
	    :documentation "Acquire a new lock ID.")

(def-function ("db_env_lock_id_free" %db-env-lock-id-free)
    ((env :pointer-void)
     (id :unsigned-int))
  :returning :int)

(wrap-errno db-env-lock-id-free (env id)
	    :documentation "Release a lock ID.")

(def-function ("db_env_lock_get" %db-env-lock-get)
    ((env :pointer-void)
     (locker :unsigned-int)
     (flags :unsigned-int)
     (object array-or-pointer-char)
     (object-size :unsigned-int)
     (lock-mode DB-LOCKMODE)
     (lock (* DB-LOCK)))
  :returning :int)

(wrap-errno db-env-lock-get (env locker flags object object-size
				 lock-mode lock)
	    :flags (lock-nowait)
	    :documentation "Acquire a lock.")

(def-function ("db_env_lock_put" %db-env-lock-put)
    ((env :pointer-void)
     (lock (* DB-LOCK)))
  :returning :int)

(wrap-errno db-env-lock-put (env lock) 
	    :documentation "Release a lock.")

(defmacro with-lock ((env locker object object-size 
			  &key (lock-mode DB-LOCKMODE#WRITE)
			  lock-nowait)
		     &body body)
  "Execute the body with a lock held, releasing unconditionally."
  (let ((lock (gensym))
	(locked (gensym)))
  `(with-foreign-object (,lock 'DB-LOCK)
    (let ((,locked nil))
      (unwind-protect
	   (progn 
	     (db-env-lock-get ,env ,locker ,object ,object-size ,lock-mode
			      ,lock :lock-nowait ,lock-nowait)
	     (setq ,locked T)
	     ,@body)
	(when ,locked (db-env-lock-put ,env ,lock)))))))      
     
(def-function ("db_env_lock_vec" %db-env-lock-vec)
    ((env :pointer-void)
     (locker :unsigned-int)
     (flags :unsigned-int)
     (list (:array DB-LOCKREQ))
     (nlist :int)
     (elistp (* (* DB-LOCKREQ))))
  :returning :int)

(def-function ("db_env_set_timeout" %db-env-set-timeout)
    ((env :pointer-void)
     (timeout :unsigned-int)
     (flags :unsigned-int))
  :returning :int)

(wrap-errno db-env-set-timeout (env timeout flags)
	    :flags (set-lock-timeout set-transaction-timeout)
	    :documentation 
"Set a timeouts on locks and transactions.  If you set this,
be prepared to handle deadlock / lock no granted errors.")

(def-function ("db_env_get_timeout" %db-env-get-timeout)
    ((env :pointer-void)
     (timeout :unsigned-int :out)
     (flags :unsigned-int))
  :returning :int)

(wrap-errno db-env-get-timeout (env flags) :outs 2
	    :flags (set-lock-timeout set-transaction-timeout)
	    :documentation "Gets the timout.")

(def-function ("db_env_set_cachesize" %db-env-set-cachesize)
    ((env :pointer-void)
     (gbytes :unsigned-int)
     (bytes :unsigned-int)
     (ncache :int))
  :returning :int)

(wrap-errno db-env-set-cachesize (env gbytes bytes ncache) 
	    :documentation "Sets the size of the buffer pool cache
            for elephant database data.  Set large if you can!")

(def-function ("db_env_get_cachesize" %db-env-get-cachesize)
    ((env :pointer-void)
     (gbytes :unsigned-int :out)
     (bytes :unsigned-int :out)
     (ncache :int :out))
  :returning :int)

(wrap-errno db-env-get-cachesize (env) :outs 4
	    :documentation "Return the current cache size of
            the BDB environment buffer pool")

(def-function ("db_env_set_lk_detect" %db-env-set-lock-detect)
    ((env :pointer-void)
     (detect :unsigned-int))
  :returning :int)

(wrap-errno db-env-set-lock-detect (env detect) 
	    :documentation 
"Set whether (or not) to run the deadlock detector on every
time there is a conflict.")

(def-function ("db_env_get_lk_detect" %db-env-get-lock-detect)
    ((env :pointer-void)
     (detect :unsigned-int :out))
  :returning :int)

(wrap-errno db-env-get-lock-detect (env) :outs 2 :documentation
"Get whether the deadlock detector is run on every conflict.")

(def-function ("db_env_lock_detect" %db-env-lock-detect)
    ((env :pointer-void)
     (flags :unsigned-int)
     (atype :unsigned-int)
     (aborted :int :out))
  :returning :int)

(wrap-errno db-env-lock-detect (env flags atype) :outs 2 :documentation
"Run one iteration of the deadlock detector.")

;; Secondary indices

(def-function ("db_associate" %db-associate)
    ((primary :pointer-void)
     (txn :pointer-void)
     (secondary :pointer-void)
     (callback :pointer-void)
     (flags :unsigned-int))
  :returning :int)

(wrap-errno db-associate (primary transaction secondary callback flags)
	    :flags (create)
	    :keys ((transaction (txn-default *current-transaction*)))
	    :transaction transaction
	    :documentation 
"Assocate a DB as a secondary index of another DB.  Takes a
callback function which generates secondary keys.")

;; Some C Hacks for Elephant

(def-function ("db_fake_associate" %db-fake-associate)
    ((primary :pointer-void)
     (txn :pointer-void)
     (secondary :pointer-void)
     (flags :unsigned-int))
  :returning :int)

(wrap-errno db-fake-associate (primary transaction secondary flags)
	    :flags (auto-commit)
	    :keys ((transaction (txn-default *current-transaction*)))
	    :transaction transaction
	    :documentation
"Assocates a DB as a secondary index of another DB.  Uses a
no-op function to generate secondary indices (assuming the
user will manually maintain the secondary index.)")

(def-function ("db_set_bt_compare" %db-set-bt-compare)
    ((db :pointer-void)
     (compare-function :pointer-void))
  :returning :int)

(def-function ("db_set_lisp_compare" %db-set-lisp-compare)
    ((db :pointer-void)
     (version :int))
  :returning :int)

(wrap-errno db-set-lisp-compare (db version) :documentation 
"Sets the Btree comparision function to a hand-cooked
function for Elephant to compare lisp values.")

(def-function ("db_set_dup_compare" %db-set-dup-compare)
    ((db :pointer-void)
     (dup-function :pointer-void))
  :returning :int)

(def-function ("db_set_lisp_dup_compare" %db-set-lisp-dup-compare)
    ((db :pointer-void)
     (version :int))
  :returning :int)

(wrap-errno db-set-lisp-dup-compare (db version) :documentation 
"Sets the duplicate comparision function to a hand-cooked
function for Elephant to compare lisp values.")

;; Sequences

(def-function ("db_sequence_create2" %db-sequence-create)
    ((db :pointer-void)
     (flags :unsigned-int)
     (errno (* :int)))
  :returning :pointer-void)

(defun db-sequence-create (db)
  "Create a new sequence."
  (declare (type pointer-void db))
  (let ((errno-buffer (allocate-foreign-object :int 1)))
    (declare (type pointer-int errno-buffer))
    (let* ((seq
	    (%db-sequence-create db 0 errno-buffer))
	   (errno (deref-array errno-buffer '(:array :int) 0)))
      (declare (type pointer-void seq)
	       (type fixnum errno))
      (if (= errno 0) 
	  seq
	  (error 'db-error :errno errno)))))

(def-function ("db_sequence_open" %db-sequence-open)
    ((seq :pointer-void)
     (txn :pointer-void)
     (key :cstring)
     (key-size :unsigned-int)
     (flags :unsigned-int))
  :returning :int)

(wrap-errno db-sequence-open (sequence transaction key key-size flags)
	    :flags (create excl thread)
	    :cstrings (key)
	    :keys ((key-size (length key))
		   (transaction (txn-default *current-transaction*)))
	    :transaction transaction
	    :documentation "Open a sequence.")	    

(def-function ("db_sequence_close" %db-sequence-close)
    ((seq :pointer-void)
     (flags :unsigned-int))
  :returning :int)

(wrap-errno (db-sequence-close %db-sequence-close) (sequence flags)
	    :documentation "Close a sequence.")

(def-function ("db_sequence_get" %db-sequence-get)
    ((seq :pointer-void)
     (txn :pointer-void)
     (delta :int)
     (low :unsigned-int :out)
     (high :int :out)
     (flags :unsigned-int))
  :returning :int)

(defun db-sequence-get (sequence delta &key txn-nosync 
			(transaction (txn-default *current-transaction*)))
  "Get the next element."
  (declare (type pointer-void sequence transaction)
	   (type fixnum delta)
	   (type boolean txn-nosync))
  (multiple-value-bind
	(errno low high)
      (%db-sequence-get sequence transaction delta
			(flags :txn-nosync txn-nosync))
    (declare (type fixnum errno)
	     (type (unsigned-byte 32) low)
	     (type (signed-byte 32) high))
    (cond ((= errno 0) (make-64-bit-integer high low))
	  ((or (= errno db_lock_deadlock)
	       (= errno db_lock_notgranted))
	   (throw 'transaction transaction))
	  (t (error 'db-error :errno errno)))))

(def-function ("db_sequence_get_lower" %db-sequence-get-lower)
    ((seq :pointer-void)
     (txn :pointer-void)
     (delta :int)
     (low :int :out)
     (flags :unsigned-int))
  :returning :int)

(defun db-sequence-get-fixnum (sequence delta &key txn-nosync 
			       (transaction (txn-default *current-transaction*)))
  "Get the next element as a fixnum."
  (declare (type pointer-void sequence transaction)
	   (type fixnum delta)
	   (type boolean txn-nosync))
  (multiple-value-bind
	(errno low)
      (%db-sequence-get-lower sequence transaction delta
			      (flags :txn-nosync txn-nosync))
    (declare (type fixnum errno low))
    (cond ((= errno 0) low)
	  ((or (= errno db_lock_deadlock)
	       (= errno db_lock_notgranted))
	   (throw 'transaction transaction))
	  (t (error 'db-error :errno errno)))))

(def-function ("db_sequence_initial_value" %db-sequence-initial-value)
    ((seq :pointer-void)
     (low :unsigned-int)
     (high :int))
  :returning :int)

(defun db-sequence-initial-value (sequence value)
  "Set the initial value."
  (let ((errno
	 (%db-sequence-initial-value sequence (low32 value) (high32 value))))
    (declare (type fixnum errno))
    (cond ((= errno 0) nil)
	  (t (error 'db-error :errno errno)))))

(def-function ("db_sequence_remove" %db-sequence-remove)
    ((seq :pointer-void)
     (txn :pointer-void)
     (flags :unsigned-int))
  :returning :int)

(wrap-errno db-sequence-remove (sequence transaction flags)
	    :keys ((transaction (txn-default *current-transaction*)))
	    :transaction transaction
	    :flags (txn-nosync)
	    :documentation "Remove a sequence.")

(def-function ("db_sequence_set_cachesize" %db-sequence-set-cachesize)
    ((seq :pointer-void)
     (size :int))
  :returning :int)

(wrap-errno db-sequence-set-cachesize (sequence size)
	    :documentation "Set cache size for a sequence.")

(def-function ("db_sequence_get_cachesize" %db-sequence-get-cachesize)
    ((seq :pointer-void)
     (size :int :out))
  :returning :int)

(wrap-errno db-sequence-get-cachesize (sequence)
	    :outs 2
	    :documentation "Get cache size for a sequence.")

(def-function ("db_sequence_set_flags" %db-sequence-set-flags)
    ((seq :pointer-void)
     (flags :unsigned-int))
  :returning :int)

(wrap-errno db-sequence-set-flags (sequence flags)
	    :flags (seq-dec seq-inc seq-wrap)
	    :documentation "Set cache size for a sequence.")

(def-function ("db_sequence_set_range" %db-sequence-set-range)
    ((seq :pointer-void)
     (minlow :unsigned-int)
     (minhigh :int)
     (maxlow :unsigned-int)
     (maxhigh :int))
  :returning :int)

(defun db-sequence-set-range (sequence min max)
  "Set the range of a sequence"
  (let ((errno
	 (%db-sequence-set-range sequence (low32 min) (high32 min)
				 (low32 max) (high32 max))))
    (declare (type fixnum errno))
    (cond ((= errno 0) nil)
	  (t (error 'db-error :errno errno)))))

(def-function ("db_sequence_get_range" %db-sequence-get-range)
    ((seq :pointer-void)
     (minlow :unsigned-int :out)
     (minhigh :int :out)
     (maxlow :unsigned-int :out)
     (maxhigh :int :out))
  :returning :int)

(defun db-sequence-get-range (sequence)
  "Get the range of a sequence"
  (multiple-value-bind (errno minlow minhigh maxlow maxhigh)
      (%db-sequence-get-range sequence)
    (declare (type fixnum errno)
	     (type integer minlow minhigh maxlow maxhigh))
    (cond ((= errno 0) (values (make-64-bit-integer minhigh minlow)
			       (make-64-bit-integer maxhigh maxlow)))
	  (t (error 'db-error :errno errno)))))

(def-function ("next_counter" %next-counter)
    ((env :pointer-void)
     (db :pointer-void)
     (parent :pointer-void)
     (key array-or-pointer-char)
     (key-size :unsigned-int)
     (lockid array-or-pointer-char)
     (lockid-size :unsigned-int))
  :returning :int)

(defun next-counter (env db parent key key-size lockid lockid-size)
  "Get the next element in the counter.  To be deprecated when 4.3 is released."
  (let ((ret (%next-counter env db parent key key-size lockid lockid-size)))
    (if (< ret 0)
	(error 'db-error :errno ret)
	ret)))
