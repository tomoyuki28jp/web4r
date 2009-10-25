;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;
;;; classes.lisp -- persistent objects via metaobjects
;;; 
;;; Initial version 8/26/2004 by Andrew Blumberg
;;; <ablumberg@common-lisp.net>
;;; 
;;; part of
;;;
;;; Elephant: an object-oriented database for Common Lisp
;;;
;;; Copyright (c) 2004 by Andrew Blumberg and Ben Lee
;;; <ablumberg@common-lisp.net> <blee@common-lisp.net>
;;;
;;; Elephant users are granted the rights to distribute and use this software
;;; as governed by the terms of the Lisp Lesser GNU Public License
;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;
(in-package "ELEPHANT")

(defvar *debug-si* nil)

(defmethod initialize-instance :before  ((instance persistent)
					 &rest initargs
					 &key from-oid
					 (sc *store-controller*))
  "Sets the OID and home controller"
  (declare (ignore initargs))
  (if (null sc)
      (error "Initialize instance for type persistent requires valid store controller argument :sc"))
  (if from-oid
      (setf (oid instance) from-oid)
      (setf (oid instance) (next-oid sc)))
  (setf (:dbcn-spc-pst instance) (controller-spec sc))
  (cache-instance sc instance))

(defclass persistent-object (persistent) ()
  (:metaclass persistent-metaclass)
  (:documentation 
   "Superclass of all user-defined persistent classes.  This is
    automatically inherited if you use the persistent-metaclass
    metaclass."))

;; ================================================
;; METACLASS INITIALIZATION AND CHANGES
;; ================================================

(defmethod ensure-class-using-class :around ((class (eql nil)) name &rest args &key index)
  "Support the :index class option"
  (let ((result (apply #'call-next-method class name (remove-index-keyword args))))
    (when (and index (subtypep (type-of result) 'persistent-metaclass))
      (update-indexed-record result nil :class-indexed t))
    result))

(defmethod ensure-class-using-class :around ((class persistent-metaclass) name &rest args &key index)
  "Support the :index class option on redefinition"
  (let ((result (apply #'call-next-method class name (remove-index-keyword args))))
    (when index
      (update-indexed-record result nil :class-indexed t))
    result))
				     
(defun remove-index-keyword (list)
  (cond ((null list) 
	 nil)
	((eq (car list) :index)
	 (cddr list))
	(t 
	 (cons (car list) (remove-index-keyword (cdr list))))))

(defmethod shared-initialize :around ((class persistent-metaclass) slot-names &rest args &key direct-superclasses)
  "Ensures we inherit from persistent-object."
  (let* ((persistent-metaclass (find-class 'persistent-metaclass))
	 (persistent-object (find-class 'persistent-object))
	 (not-already-persistent (loop for superclass in direct-superclasses
				       never (eq (class-of superclass) persistent-metaclass))))
    (if (and (not (eq class persistent-object)) not-already-persistent)
	(apply #'call-next-method class slot-names
	       :direct-superclasses (cons persistent-object
					  direct-superclasses) args)
	(call-next-method))))

(defmethod finalize-inheritance :around ((instance persistent-metaclass))
  "Update the persistent slot records in the metaclass."
  (prog1
      (call-next-method)
    (when (not (slot-boundp instance '%persistent-slots))
	(setf (%persistent-slots instance) 
	      (cons (persistent-slot-names instance) nil)))
    (update-indexed-record instance (indexed-slot-names-from-defs instance))))

(defmethod reinitialize-instance :around ((instance persistent-metaclass) &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (prog1
      (call-next-method)
    (when (class-finalized-p instance)
      (update-persistent-slots instance (persistent-slot-names instance))
      (update-indexed-record instance (indexed-slot-names-from-defs instance))
      (if (removed-indexing? instance)
	  (progn 
	    (let ((class-idx (get-value (class-name instance) (controller-class-root *store-controller*))))
	      (when class-idx
		(wipe-class-indexing instance class-idx)))
	    (setf (%index-cache instance) nil))
	  (set-db-synch instance :class))
;;      #+allegro
;;      (loop with persistent-slots = (persistent-slots instance)
;;	    for slot-def in (class-direct-slots instance)
;;	    when (member (slot-definition-name slot-def) persistent-slots)
;;	    do (initialize-accessors slot-def instance))
      (make-instances-obsolete instance))))

;; ================================================
;; PERSISTENT OBJECT MAINTENANCE
;; ================================================

;;
;; CLASS INSTANCE INITIALIZATION
;;

(defmethod shared-initialize :around ((instance persistent-object) slot-names &rest initargs &key from-oid &allow-other-keys)
  "Initializes the persistent slots via initargs or forms.
This seems to be necessary because it is typical for
implementations to optimize setting the slots via initforms
and initargs in such a way that slot-value-using-class et al
aren't used.  We also handle writing any indices after the 
class is fully initialized.  Calls the next method for the transient 
slots."
  (let* ((class (find-class (class-name (class-of instance))))
	 (oid (oid instance))
	 (persistent-slot-names (persistent-slot-names class)))
    (flet ((persistent-slot-p (item) 
	     (member item persistent-slot-names :test #'eq)))
      (let ((transient-slot-inits 
	     (if (eq slot-names t)	; t means all slots
		 (transient-slot-names class)
		 (remove-if #'persistent-slot-p slot-names)))
	    (persistent-slot-inits
	     (if (eq slot-names t) 
		 persistent-slot-names
		 (remove-if-not #'persistent-slot-p slot-names))))
	(inhibit-indexing oid)
	(unwind-protect 
	     (progn
	       ;; initialize the persistent slots ourselves
	       (initialize-persistent-slots class instance persistent-slot-inits initargs)
	       ;; let the implementation initialize the transient slots
	       (apply #'call-next-method instance transient-slot-inits initargs))
	  (uninhibit-indexing oid))
	;; Inhibit indexing altogether if the object already was defined (ie being created 
	;;   from an oid) as it should be indexed already.  This hack avoids a deadlock 
	;;   situation where we write the class or index page that we are currently reading 
	;;   via a cursor without going through the cursor abstraction. There has to be a 
	;;   better way to do this.
	(when (and (indexed class) (not from-oid))
	  (let ((class-index (find-class-index class)))
	    (when class-index
	      (setf (get-value oid class-index) instance))))
	))))

(defun initialize-persistent-slots (class instance persistent-slot-inits initargs)
  (flet ((initialize-from-initarg (slot-def)
	   (loop for initarg in initargs
	      with slot-initargs = (slot-definition-initargs slot-def)
	      when (member initarg slot-initargs :test #'eq)
	      do 
		(setf (slot-value-using-class class instance slot-def) 
		      (getf initargs initarg))
		(return t))))
    (with-transaction (:store-controller (get-con instance))
      (loop for slot-def in (class-slots class)
	 unless (initialize-from-initarg slot-def)
	 when (member (slot-definition-name slot-def) persistent-slot-inits :test #'eq)
	 unless (slot-boundp-using-class class instance slot-def)
	 do
	 (let ((initfun (slot-definition-initfunction slot-def)))
	   (when initfun
	     (setf (slot-value-using-class class instance slot-def)
		   (funcall initfun))))))))

;;
;; CLASS REDEFINITION PROTOCOL
;;

(defmethod update-instance-for-redefined-class :around ((instance persistent-object) added-slots discarded-slots property-list &rest initargs &key &allow-other-keys)
  ;; NOTE: probably should delete discarded slots, but we'll worry about that later
  ;;       (also will want to delete discarded indices since we don't have a good GC)
  (declare (ignore property-list discarded-slots added-slots))
  (prog1
      (call-next-method)
    (let* ((class (class-of instance))
	   (new-persistent-slots (set-difference (persistent-slots class)
						 (old-persistent-slots class))))
      ;; Update new persistent slots, the others we get for free (same oid!)
      ;; Isn't this done by the default call-next-method?
      (apply #'shared-initialize instance new-persistent-slots initargs))
    ))

;;
;; CLASS CHANGE PROTOCOL
;;

(defmethod update-instance-for-different-class :around ((previous persistent) (current persistent) &rest initargs &key)
  (let* ((old-class (class-of previous))
	 (new-class (class-of current))
	 (new-persistent-slots (set-difference
				(persistent-slots new-class)
				(persistent-slots old-class)))
	 (raw-retained-persistent-slots (intersection (persistent-slots new-class)
						      (persistent-slots old-class)))
	 (retained-unbound-slots (loop for slot-name in raw-retained-persistent-slots
				       when (not (persistent-slot-boundp (get-con previous) previous slot-name))
				       collect slot-name))
 	 (retained-persistent-slots (set-difference raw-retained-persistent-slots retained-unbound-slots)))
    ;; Apply default values for unbound & new slots (updates class index)
    (apply #'shared-initialize current (append new-persistent-slots retained-unbound-slots) initargs)
    ;; Copy values from old class (NOTE: should delete discarded slots?) (updates class index)
    (with-transaction (:store-controller (get-con current))
      (loop for slot-def in (class-slots new-class)
	 when (member (slot-definition-name slot-def) retained-persistent-slots)
	 do (setf (slot-value-using-class new-class
					  current
					  slot-def)
		  (slot-value-using-class old-class
					  previous
					  (find-slot-def-by-name old-class (slot-definition-name slot-def))))))
    ;; Delete this instance from its old class index, if exists
    (when (indexed old-class)
      (remove-kv (oid previous) (find-class-index old-class)))
    (call-next-method)))


;;
;; SLOT ACCESS PROTOCOLS
;;

(defmethod slot-value-using-class :around ((class persistent-metaclass) (instance persistent-object) (slot-def persistent-slot-definition))
  "Get the slot value from the database."
  (declare (optimize (speed 3)))
  (let ((name (slot-definition-name slot-def)))
    (persistent-slot-reader (get-con instance) instance name)))

(defmethod (setf slot-value-using-class) :around (new-value (class persistent-metaclass) (instance persistent-object) (slot-def persistent-slot-definition))
  "Set the slot value in the database."
  (declare (optimize (speed 3)))
  (if (indexed class)
      (indexed-slot-writer class instance slot-def new-value)
      (let ((name (slot-definition-name slot-def)))
	(persistent-slot-writer (get-con instance) new-value instance name))))

(defmethod slot-boundp-using-class :around ((class persistent-metaclass) (instance persistent-object) (slot-def persistent-slot-definition))
  "Checks if the slot exists in the database."
  (declare (optimize (speed 3)))
  (let ((name (slot-definition-name slot-def)))
    (persistent-slot-boundp (get-con instance) instance name)))

(defmethod slot-boundp-using-class :around ((class persistent-metaclass) (instance persistent-object) (slot-name symbol))
  "Checks if the slot exists in the database."
  (declare (optimize (speed 3)))
  (loop for slot in (class-slots class)
	for matches-p = (eq (slot-definition-name slot) slot-name)
	until matches-p
	finally (return (if (and matches-p
				 (subtypep (type-of slot) 'persistent-slot-definition))
			    (persistent-slot-boundp (get-con instance) instance slot-name)
			    (call-next-method)))))

(defmethod slot-makunbound-using-class :around ((class persistent-metaclass) (instance persistent-object) (slot-def persistent-slot-definition))
  "Deletes the slot from the database."
  (declare (optimize (speed 3)))
  ;; NOTE: call remove-indexed-slot here instead?
  (when (indexed slot-def)
    (unregister-indexed-slot class (slot-definition-name slot-def)))
  (persistent-slot-makunbound (get-con instance) instance (slot-definition-name slot-def)))
