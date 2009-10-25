;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;
;;; sql-transactions.lisp -- Top level transaction support for elephant sql
;;; 
;;; Initial version 10/12/2005 by Robert L. Read
;;; <read@robertlread.net>
;;; 
;;; part of
;;;
;;; Elephant: an object-oriented database for Common Lisp
;;;
;;; Copyright (c) 2005-2007 by Robert L. Read
;;; Portions Copyright (c) 2006-2007 by Ian Eslick
;;;
;;; Elephant users are granted the rights to distribute and use this software
;;; as governed by the terms of the Lisp Lesser GNU Public License
;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;

(in-package :db-clsql)

(defmethod execute-transaction ((sc sql-store-controller) txn-fn &key &allow-other-keys)
  "Execute a body with a transaction in place.  On success,
   the transaction is committed.  Otherwise, the transaction is
   aborted.  If the body deadlocks, the body is re-executed in
   a new transaction, retrying a fixed number of iterations.
   *auto-commit* is false for the body of the transaction."
  ;; SQL doesn't support nested transaction so we lump it all 
  ;; together
  (if (clsql::in-transaction-p :database (controller-db sc))
      (funcall txn-fn)
      (progn
	(clsql::set-autocommit nil)
	(let ((db (controller-db sc)))
	  (unwind-protect
	       (multiple-value-prog1
		   (progn
		     (clsql-sys::database-start-transaction db)
		     (funcall txn-fn))
		 (clsql-sys::mark-transaction-committed db))
	    (if (eq (clsql-sys::transaction-status (clsql-sys::transaction db)) :committed)
		(clsql-sys::database-commit-transaction db)
		(clsql-sys::database-abort-transaction db))
	    (clsql::set-autocommit t))))))

(defmethod controller-start-transaction ((sc sql-store-controller) &key &allow-other-keys)
  (clsql:start-transaction :database (controller-db sc))
  'active-clsql-transaction)

(defmethod controller-commit-transaction ((sc sql-store-controller) transaction &key &allow-other-keys)
  (declare (ignore transaction))
  (clsql:commit :database (controller-db sc)))

(defmethod controller-abort-transaction ((sc sql-store-controller) transaction &key &allow-other-keys)
  (declare (ignore transaction))
  (clsql:rollback :database (controller-db sc)))

