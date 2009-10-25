;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;
;;; classindex-untils.lisp -- support for classindex.lisp and
;;;                           class re-definition synchronization
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

(in-package :elephant)

(declaim #-elephant-without-optimize (optimize (speed 3) (safety 1)))

;;
;; Simple utilities for managing synchronization between class
;; definitions and database state
;;

(defparameter *default-indexed-class-synch-policy* :class
  "[:union | :db | :class] determines which reference defines
   the indexing structure after a reconnect to a persistent
   store.  If the class is redefined, the default is that the
   class dominates.  Changing this parameter alters the 
   default behavior to :union (merge indexed slots from database
   and class definition) or :db which changes the indexing of
   the class to match the db.  This can fail in several ways:
   a) the class does not have a persistent slot defined for
      a slot index (will be treated as derived & fail on write)
   b) A slot has been added with the name of a derived index
      this will be confusing
   c) The key-slot function definitions (if not an anoymous
      lambda) may have changed leading to unexpected indexing")

(defmethod class-index-cached? ((class persistent-metaclass))
  (and (slot-boundp class '%index-cache)
       (subtypep (type-of (%index-cache class)) 'btree)))

(defmethod determine-synch-method ((class persistent-metaclass))
  "This method should be called on the class if the %index-cache slot is
   not a subtype of class btree to determine what synch method to call
   on the current database btree.  If DB doesn't exist, then you can ignore this"
  (cond ((not (slot-boundp class '%index-cache))
	 *default-indexed-class-synch-policy*)
	((member (%index-cache class) '(:class :union :db))
	 (%index-cache class))
	(t *default-indexed-class-synch-policy*)))

(defmethod set-db-synch ((class persistent-metaclass) method)
  "Tell the class the synch method to use to synchronize the class indices
   and the current class definition"
  (assert (member method '(:class :db :union)))
  (setf (%index-cache class) method))

;;
;; Differentiate derived indices from slot-based ones
;;

(defparameter *derived-index-marker* "%%DERIVED%%-")

(defun make-derived-name (name)
  (intern (format nil "~A~A" *derived-index-marker* name)))

(defun derived-name? (name)
  (when (symbolp name) (setf name (symbol-name name)))
  (string= (subseq name 0 (min (length name)
			       (length *derived-index-marker*)))
	   *derived-index-marker*))

(defun get-derived-name-root (dname)
  (when (symbolp dname) 
    (setf dname (symbol-name dname)))
  (intern (subseq dname (length *derived-index-marker*))))

;;
;; Interface fn for slot key forms
;;

(defun make-slot-key-form (class name)
  (assert (member name (car (%persistent-slots class))))
  `(lambda (slot-index primary instance)
     (declare (ignore slot-index primary))
     (read-slot-for-index ',(class-name class) ',name instance)))

(defun read-slot-for-index (class-name slot-name instance)
  (let ((class (find-class class-name)))
    (multiple-value-bind (found? slot-def) (find-effective-slot-def class slot-name)
      (when (and found?
		 (slot-boundp-using-class class instance slot-def))
	(values t (persistent-slot-reader (get-con instance) instance slot-name))))))

(defun find-effective-slot-def (class slot-name)
  (loop for slot in (class-slots class) do
     (when (eq (slot-definition-name slot) slot-name) 
       (return (values t slot)))))


;;
;; Simplify the computations for derived parameters
;;

(defun make-derived-key-form (dform)
  "Change the index function interface for derived class slotsw
   to better handle the various use cases.  The provided function
   accepts a single argument, the class instance to comput a 
   dervied parameter against.  Dervied indices can
   specify that the result should not be indexed by returning
   two values (values nil t) the second of which is an ignore
   specifier.  Normal functions just return the value which is
   an implicit index command.  Accessors that compute against
   unbound slots are silently ignored (ie initialization) and
   errors of other types produce warnings and are ignored.  This
   handles both named functions and anonymous lambdas."
  `(lambda (slot-index primary instance)
     (declare (ignore slot-index primary))
     (compute-derived-key-result instance #',dform)))

(defun compute-derived-key-result (instance fn)
  (handler-case 
      (multiple-value-bind (val ignore)
	  (funcall fn instance)
	(if ignore 
	    (values nil nil)
	    (values t val)))
    (unbound-slot () 
      (values nil nil))
    (error (e) 
      (warn "Error ~A computing derived index for on instance ~A" e instance)
      (values nil nil))))
  
  
;; =============================
;;  CLASS / DB SYNCHRONIZATION
;; =============================

;; TO READER:  I got really tired of trying to figure out all
;; the messy conditionals and I figure default behaviors are something
;; others might want to modify, so here's what determines the rule 
;; behavior..
;; 
;; Rules match on the following states of the metaclass and current
;; database class-index for each slotname currently in either of
;; those sources.  Actions are taken, typically when a slot exists
;; in one but not the other or features like indexed/persistent 
;; differ between the slots
;; 
;; class state:
;;   class-indexed - the slot is marked as indexed
;;   class-persistent - the slot is marked as persistent (not indexed)
;;   class-transient - the slot is marked transient
;;   class-derived - the slot is in the derived list of the class
;;
;; database
;;   db-slot - the database has a slot index
;;   db-derived - the database has a derived index
;;
;; The inversions of any of these terms are also available as
;; (not indexed-slot) for example, to cover more than one feature 
;; combination
;;
;; Each rule should apply uniquely to a given feature set.
;;
;; Actions taken when rules match can include:
;;
;; add-slot-index - add a new index with the slotname to the db
;; remove-slot-index - remove a slot with the slotname from the db
;; add-derived-index - xxx this makes no sense! xxx
;; remove-derived-index - remove a derived index from the db
;; unregister-indexed-slot - remove an indexed slot from the class metaobject
;; unregister-derived - remove a derived index from the class metaobject
;; register-indexed-slot - register a slot with the class metaobject
;; register-derived-index - register a derived index with the class metaobject
;;

;; DEFINE THE SYNCHRONIZATION RULES
(eval-when (:compile-toplevel :load-toplevel)

  (defclass synch-rule ()
    ((lhs :accessor synch-rule-lhs :initarg :lhs :initform nil)
     (rhs :accessor synch-rule-rhs :initarg :rhs :initform nil)))

  (defun make-synch-rule (rule-spec)
    (let ((lhs (subseq rule-spec 0 (position '=> rule-spec)))
	  (rhs (subseq rule-spec (1+ (position '=> rule-spec)))))
      (make-instance 'synch-rule :lhs lhs :rhs rhs)))

  (defparameter *synchronize-rules* 
    (mapcar #'(lambda (rule-specs)
		(cons (car rule-specs)
		      (mapcar #'make-synch-rule (cdr rule-specs))))
	    '((:class ;; class overwrites db
	       ((not db-slot) class-indexed => add-slot-index)
	       (db-slot (not class-indexed) => remove-slot-index)
	       (db-derived (not class-indexed) (not class-persistent)
		           (not class-transient) => register-derived-index))
	      (:union ;; merge both sides - conflicts favor class
	       (db-slot (not class-indexed) => register-indexed-slot)
	       ((not db-slot) class-indexed => add-slot-index)
	       (db-derived (not class-derived) => register-derived-index)
	       (db-derived class-persistent => remove-derived-index warn))
	      (:db 
	       ;; :db updates indexing in classes when indexes and
	       ;; slots overlapped there may still be problems with
	       ;; derived indices that refer to missing slots or
	       ;; conflict with new slotnames
	       ((not db-slot) class-indexed => unregister-indexed-slot)
	       ((not db-derived) class-derived => unregister-derived-index)
	       (db-slot class-persistent => register-indexed-slot)
	       (db-slot class-transient => remove-indexed-slot)
	       (db-derived class-transient => remove-derived-index warn)
	       (db-derived class-persistent => remove-derived-index warn)
	       (db-derived class-indexed => remove-derived-index warn)
               (db-derived (not class-derived) (not class-indexed) 
		           (not class-persistent) (not class-transient) 
		           => register-derived-index)))))
  )

;; TOP LEVEL METHOD

(defun synchronize-class-to-store (class &key (sc *store-controller*) 
				   (method *default-indexed-class-synch-policy*))
  (let ((slot-records (compute-class-and-ele-status class sc))
	(rule-set (cdr (assoc method *synchronize-rules*))))
    (apply-synch-rules class slot-records rule-set)))

;; COMPUTING RULE APPLICABILITY AND FIRING

(defun synch-rule-applicable? (rule features)
  (simple-match-set (synch-rule-lhs rule) features))

(defun simple-match-set (a b)
  (cond ((null a) t)
	((and (not (null a)) (null b)) nil)
	((member (first a) b :test #'equal)
	 (simple-match-set (cdr a) (remove (first a) b :test #'equal)))
	(t nil)))

(defparameter *print-synch-messages* nil)

(defun apply-synch-rule (rule class name)
  (when *print-synch-messages*
    (format t "Class/DB Synch: converting state ~A using ~A for ~A~%" 
	    (synch-rule-lhs rule) (synch-rule-rhs rule) name))
  (loop for action in (synch-rule-rhs rule) do
       (case action
	 (add-slot-index (add-class-slot-index class name :update-class nil))
	 (remove-slot-index (remove-class-slot-index class name :update-class nil))
         (add-derived-index (add-class-derived-index class name :update-class nil))
	 (remove-derived-index (remove-class-derived-index class name :update-class nil))
	 (unregister-indexed-slot (unregister-indexed-slot class name))
	 (unregister-derived-index (unregister-derived-index class name))
	 (register-indexed-slot (register-indexed-slot class name))
	 (register-derived-index (register-derived-index class name))
	 (warn (warn "Performing slot synchronization actions: ~A" (synch-rule-rhs rule))))))

(defun apply-synch-rules (class records rule-set)
  (labels ((slotname (rec) (car rec))
	   (feature-set (rec) (cdr rec)))
    (loop for record in records do
	 (loop for rule in rule-set
	       when (synch-rule-applicable? rule (feature-set record)) 
	    do
	       (apply-synch-rule rule class (slotname record))))))

;; COMPUTE CURRENT STATE OF CLASS OBJECT AND DATABASE AFTER CHANGES

(defun compute-class-and-ele-status (class &optional (store-controller *store-controller*))
  (let* ((*store-controller* store-controller)
	 ;; db info
	 (db-indices (find-inverted-index-names class))
	 (db-derived (remove-if-not #'derived-name? db-indices))
	 (db-slot (set-difference db-indices db-derived))
	 ;; class info
	 (marked-slots (indexing-record-slots (indexed-record class)))
	 (marked-derived (indexing-record-derived (indexed-record class)))
	 (persistent-slots (set-difference (persistent-slots class) marked-slots))
	 (other-slots (set-difference
		       (set-difference (class-slots class) persistent-slots)
		       marked-slots))
	 (all-names (union (mapcar #'slot-definition-name (class-slots class)) db-indices))
	 ;; [order matters in traversal]
	 (all-sets `((class-indexed . ,marked-slots)
		     (class-derived . ,marked-derived)
		     (class-persistent . ,persistent-slots)
		     (class-transient . ,other-slots)
		     (db-slot . ,db-slot)
		     (db-derived . ,db-derived))))
    (labels ((compute-features (slotname)
	       (let ((features nil))
		 (loop for set in all-sets do
		      (push (compute-feature slotname (cdr set) (car set))
			    features))
		 (cons slotname features)))
	     (compute-feature (name set label)
	       (if (member name set)
		   label
		   `(not ,label))))
      (mapcar #'compute-features all-names))))

;; ==================================
;;              TOOLS
;; ==================================


;;
;; This has turned out to be useful for debugging
;; 


(defun describe-db-class-index (class-name &key (sc *store-controller*))
  (let ((class-idx (find-class-index class-name :sc sc)))
    (if class-idx
	(let ((names nil))
	  (map-indices (lambda (k v)
			 (declare (ignore v))
			 (push k names))
		       class-idx)
	  (format t "Class Index: ~A~%" class-name)
	  (format t "~{~A~%~}" (nreverse names)))
	(format t "No persistent index for class ~A.~%" class-name))))

(defun wipe-indexed-class (name)
  (ignore-errors
    (disable-class-indexing name)
    (flush-instance-cache *store-controller*)
    (setf (find-class name) nil)))


;; Rob created this just for some debugging.
;; It seems theoretically possible that we could make
;; a function that fully checks the consinstency of the index;
;; that is, that the indexed classes indeed exist in the store.
(defun dump-class-index (c)
  (let ((idx (find-class-index c)))
    (dump-btree
     idx)
    )
  )

(defun report-indexed-classes (&key (class nil) (sc *store-controller*))
  (format t "indexed-classes:~%")
  (let ((bt (controller-class-root sc)))
    (declare (type btree bt))
    (dump-btree bt)
    (if class 
	(dump-class-index class)
	(map-btree
	 #'(lambda (k v) 
	     (declare (ignore v))
	     (dump-class-index k)
	     )
	 bt))
    )
  )
