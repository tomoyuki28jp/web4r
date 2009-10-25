;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;
;;; transactions.lisp -- Top level transaction support for elephant
;;; 
;;; By Ian Eslick <ieslick common-lisp net>
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

(defgeneric execute-transaction (store-controller txn-fn &rest rest &key &allow-other-keys)
  (:documentation 
   "This is an interface to the backend's transaction function.  The
    body should be executed in a dynamic environment that protects against
    non-local exist, provides ACID properties for DB operations within the
    body and properly binds any relevant parameters."))

;; Transaction architecture:
;;
;; User and designer considerations:
;; - *current-transaction* is reserved for use by dynamic transaction context.  The default global
;;   value must always be null (no transaction).  Each backend can set it to a different parameter
;;   within the dynamic context of an execute-transaction.
;; - Any closures returned from within a transaction cannot bind *current-transaction*
;; - Only a normal return value will result in the transaction being committed, any non-local exit
;;   results in a transaction abort.  If you want to do something more sophisticated, roll your own
;;   using controller-start-transaction, etc.
;; - The body of a with or ensure transaction can take any action (throw, signal, error, etc)
;;   knowing that the transaction will be aborted
;;
;; Designer considerations:
;; - with-transaction passes *current-transaction* or the user parameter to execute-transaction
;;   in the parent keyword argument.  Backends allowing nested transactions can treat the transaction
;;   as a parent, otherwise they can reuse the current transaction by ignoring it (inheriting the dynamic
;;   value of *current-transaction*) or rebinding the dynamic context (whatever makes coding easier).
;; - ensure-transaction uses *current-transaction* to determine if there is a current transaction
;;   in progress.  If so, it jumps to the body directly.  Otherwise it executes the body in a 
;;   new transaction.
;; - execute-transaction contract:
;;   - Backends must dynamically bind *current-transaction* to a meaningful identifier for the 
;;     transaction in progress and execute the provided closure in that context
;;   - All non-local exists result in an abort; only regular return values result in a commit
;;   - If a transaction is aborted due to a deadlock or read conflict, execute-transaction should 
;;     automatically retry with an appropriate default amount
;;   - execute-transaction can take any number of backend-defined keywords, although designers should 
;;     make sure there are no semantic conflicts if there is a name overlap with existing backends
;; - A typical design approach is to make sure that the most primitive interfaces to the backend 
;;   database look at *current-transaction* to determine whether a transaction is active.  Users code can also
;;   access this parameter to check whether a transaction is active.
;;
;; Multiple store considerations:
;; - When operating with multiple stores, nested transactions and BDB there are some subtle issues to
;;   work around: how to avoid writing one store with a transaction created in the context of another.
;; - For many leaf functions: *store-controller* and *current-transaction* have to both be correct;
;;   this requirement may relax in the future
;; - The following macros accomodate multiple stores by requiring that execute-transaction return a
;;   pair of (store-controller . txn-obj) where txn-obj is owned by the backend and the store-controller
;;   is the store instance it is associated with.  A nested or ensured transaction is only indicated
;;   in the call to execute transaction if the store controllers match, otherwise a new transaction
;;   for that store is created

(defun make-transaction-record (sc txn)
  "Backends must use this to assign values to *current-transaction* binding"
  (cons sc txn))

(defun transaction-store (txnrec)
  "Get the store that owns the transaction from a transaction record"
  (car txnrec))

;;(define-compiler-macro transaction-store (&whole form arg)
;;  (if (atom arg)
;;      `(car ,arg)
;;      form))

(defun transaction-object (txnrec)
  "Get the backend-specific transaction object"
  (cdr txnrec))

;;(define-compiler-macro transaction-object (&whole form arg)
;;  (if (atom arg)
;;      `(cdr ,arg)
;;      form))

(defun transaction-object-p (txnrec)
  (and (not (null txnrec))
       (consp txnrec)
       (subtypep (type-of (car txnrec)) 'store-controller)))

(defun owned-txn-p (sc parent-txn-rec)
  (and parent-txn-rec
       (transaction-object-p parent-txn-rec)
       (eq sc (transaction-store parent-txn-rec))))

(define-condition transaction-retry-count-exceeded ()
  ((retry-count :initarg :count)))

(defmacro with-transaction ((&rest keyargs &key 
				   (store-controller '*store-controller*)
				   (parent '*current-transaction*)
				   (retries 200)
				   &allow-other-keys)
			    &body body)
  "Execute a body with a transaction in place.  On success,
   the transaction is committed.  Otherwise, the transaction is
   aborted.  If the body deadlocks, the body is re-executed in
   a new transaction, retrying a fixed number of iterations.
   If nested, the backend must support nested transactions."
  (let ((sc (gensym)))
    `(let ((,sc ,store-controller))
       (funcall #'execute-transaction ,sc
		(lambda () ,@body)
		:parent (if (owned-txn-p ,sc ,parent)
			    (transaction-object ,parent)
			    nil)
		:retries ,retries
		,@(remove-keywords '(:store-controller :parent :retries)
				   keyargs)))))

(defmacro ensure-transaction ((&rest keyargs &key
				     (store-controller '*store-controller*)
				     (parent '*current-transaction*)
				     (retries 200)
				     &allow-other-keys)
			      &body body)
  "Execute the body with the existing transaction, or a new transaction if
   none is currently running.  This allows sequences of database actions to 
   be run atomically whether there is or is not an existing transaction 
   (rather than relying on auto-commit).  with-transaction nests transactions
   where as ensure-transaction can be part of an enclosing, flat transaction"
  (let ((txn-fn (gensym))
	(sc (gensym)))
    `(let ((,txn-fn (lambda () ,@body))
	   (,sc ,store-controller))
       (if (owned-txn-p ,sc ,parent)
	   (funcall ,txn-fn)
	   (funcall #'execute-transaction ,sc
		  ,txn-fn
		  :parent nil
		  :retries ,retries
		  ,@(remove-keywords '(:store-controller :parent :transaction :retries)
				   keyargs))))))

(defmacro with-batch-transaction ((batch size list &rest txn-options) &body body)
  "Perform a set of DB operations over a list of elements in batches of size 'size'.
   Pass specific transaction options after the list reference."
  `(loop for ,batch in (subsets ,size ,list) do
	(with-transaction ,txn-options
	  ,@body)))

;;
;; An interface to manage transactions explicitly
;;

;; Controller methods to implement

(defgeneric controller-start-transaction (store-controller &key &allow-other-keys)
  (:documentation "Start an elephant transaction"))

(defgeneric controller-commit-transaction (store-controller transaction &key &allow-other-keys)
  (:documentation "Commit an elephant transaction"))

(defgeneric controller-abort-transaction (store-controller transaction &key &allow-other-keys)
  (:documentation "Abort an elephant transaction"))
