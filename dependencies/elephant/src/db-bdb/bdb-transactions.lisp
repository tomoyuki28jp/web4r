;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;
;;; bdb-transactions.lisp -- Transaction support for Berkeley DB
;;; 
;;; By Ian Eslick, <ieslick common-lisp net>
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

(defmethod execute-transaction ((sc bdb-store-controller) txn-fn
				&key 
				transaction parent environment
				(retries 100) 
				degree-2 read-uncommitted txn-nosync txn-nowait txn-sync)
  (let ((env (if environment environment (controller-environment sc))))
    (loop 
       for count fixnum from 1 to retries
       for success of-type boolean = nil
       do
       (let ((txn (db-transaction-begin env
					:parent (if parent parent +NULL-VOID+)
					:degree-2 degree-2
					:read-uncommitted read-uncommitted
					:txn-nosync txn-nosync
					:txn-nowait txn-nowait
					:txn-sync txn-sync)))
	 (declare (type pointer-void txn))
	 (let (result)
	   (let ((*current-transaction* (make-transaction-record sc txn))
		 (*store-controller* sc))
	     (declare (special *current-transaction* *store-controller*))
	     (catch 'transaction
	       (unwind-protect
		    (progn
		      (setf result (multiple-value-list (funcall txn-fn)))
		      (db-transaction-commit txn 
					     :txn-nosync txn-nosync
					     :txn-sync txn-sync)
		      (setq success t))
		 (unless success 
		   (db-transaction-abort txn)))))
	   (when success
	     (return (values-list result)))))
       finally (cerror "Retry transaction again?"
		       'transaction-retry-count-exceeded
		       :format-control "Transaction exceeded the ~A retries limit"
		       :format-arguments (list retries)
		       :count retries))))
		       
    
(defmethod controller-start-transaction ((sc bdb-store-controller)
					 &key 
					 parent
					 txn-nosync
					 txn-nowait
					 txn-sync
					 read-uncommitted
					 degree-2
					 &allow-other-keys)
  (assert (not *current-transaction*))
  (db-transaction-begin (controller-environment sc)
			:parent parent
			:txn-nosync txn-nosync
			:txn-nowait txn-nowait
			:txn-sync txn-sync
			:read-uncommitted read-uncommitted
			:degree-2 degree-2))
			

(defmethod controller-commit-transaction ((sc bdb-store-controller) transaction 
					  &key txn-nosync txn-sync &allow-other-keys)
  (assert (not *current-transaction*))
  (db-transaction-commit transaction :txn-nosync txn-nosync :txn-sync txn-sync))

(defmethod controller-abort-transaction ((sc bdb-store-controller) transaction &key &allow-other-keys)
  (assert (not *current-transaction*))
  (db-transaction-abort transaction))

