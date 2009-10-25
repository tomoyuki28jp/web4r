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
;;; Original Copyright (c) 2004 by Andrew Blumberg and Ben Lee
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

(defvar *debug-si* nil)

(declaim #-elephant-without-optimize (optimize (speed 3)))

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
  (setf (dbcn-spc-pst instance) (controller-spec sc))
  (cache-instance sc instance))

(defclass persistent-object (persistent) ()
  (:metaclass persistent-metaclass)
  (:documentation 
   "Superclass for all user-defined persistent classes.  This is
    automatically inherited if you use the persistent-metaclass
    metaclass.  This allows specialization of functions for user
    objects that would not be appropriate for Elephant objects
    such as persistent collections"))

;; ================================================
;; METACLASS INITIALIZATION AND CHANGES
;; ================================================

(defmethod shared-initialize :around ((class persistent-metaclass) slot-names &rest args &key direct-superclasses index)
  "Ensures we inherit from persistent-object."
  (let* ((persistent-metaclass (find-class 'persistent-metaclass))
	 (persistent-object (find-class 'persistent-object))
	 (not-already-persistent (loop for superclass in direct-superclasses
				       never (eq (class-of superclass) persistent-metaclass))))
    (when index
      (update-indexed-record class nil :class-indexed t))
    (if (and (not (eq class persistent-object)) not-already-persistent)
	(apply #'call-next-method class slot-names
	       :direct-superclasses (append direct-superclasses (list persistent-object)) args)
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
;;      (update-indexed-record instance (indexed-slot-names-from-defs instance))
      (if (removed-indexing? instance)
	  (progn 
	    (let ((class-idx (find-class-index (class-name instance))))
	      (when class-idx
		(wipe-class-indexing instance class-idx)))
	    (setf (%index-cache instance) nil))
	  (set-db-synch instance :class))
      #+allegro
      (loop with persistent-slots = (persistent-slots instance)
	    for slot-def in (class-direct-slots instance)
	    when (member (slot-definition-name slot-def) persistent-slots)
	    do (initialize-accessors slot-def instance))
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
	       (initialize-persistent-slots class instance persistent-slot-inits initargs from-oid)
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

(defun initialize-persistent-slots (class instance persistent-slot-inits initargs object-exists)
  (flet ((initialize-from-initarg (slot-def)
	   (loop for initarg in initargs
	      with slot-initargs = (slot-definition-initargs slot-def)
	      when (member initarg slot-initargs :test #'eq)
	      do 
		(setf (slot-value-using-class class instance slot-def) 
		      (getf initargs initarg))
		(return t))))
    (ensure-transaction (:store-controller (get-con instance))
      (loop for slot-def in (class-slots class)
	 unless (initialize-from-initarg slot-def)
	 when (member (slot-definition-name slot-def) persistent-slot-inits :test #'eq)
	 unless (or object-exists (slot-boundp-using-class class instance slot-def))
	 do
	 (let ((initfun (slot-definition-initfunction slot-def)))
	   (when initfun
	     (setf (slot-value-using-class class instance slot-def)
		   (funcall initfun))))))))

;;
;; CLASS REDEFINITION PROTOCOL
;;

(defmethod update-instance-for-redefined-class :around ((instance persistent-object) added-slots discarded-slots property-list &rest initargs &key &allow-other-keys)
  (declare (ignore property-list discarded-slots added-slots))
  (prog1
      (call-next-method)
    (let* ((class (class-of instance))
	   (new-persistent-slots (set-difference (persistent-slots class)
						 (old-persistent-slots class))))
      ;; Update new persistent slots, the others we get for free (same oid!)
      ;; Isn't this done by the default call-next-method?
      (apply #'shared-initialize instance new-persistent-slots initargs)
      )
    ))

;;
;; CLASS CHANGE PROTOCOL
;;

(defmethod change-class :around ((previous persistent) (new-class standard-class) &rest initargs)
  (declare (ignorable initargs))
  (unless (subtypep (type-of new-class) 'persistent-metaclass)
    (error "Persistent instances cannot be changed to non-persistent classes in change-class"))
  (call-next-method))

(defmethod change-class :around ((previous standard-object) (new-class persistent-metaclass) &rest initargs)
  (declare (ignorable initargs))
  (unless (subtypep (type-of previous) 'persistent)
    (error "Standard classes cannot be changed to non-persistent classes in change-class"))
  (call-next-method))

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
    ;; Copy values from old class (updates class index)
    (ensure-transaction (:store-controller (get-con current))
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


;; =============================================
;; SHARED SLOT ACCESS PROTOCOL DEFINITIONS
;; =============================================

(defmethod slot-value-using-class ((class persistent-metaclass) (instance persistent-object) (slot-def persistent-slot-definition))
  "Get the slot value from the database."
  (let ((name (slot-definition-name slot-def)))
    (persistent-slot-reader (get-con instance) instance name)))

(defmethod (setf slot-value-using-class) (new-value (class persistent-metaclass) (instance persistent-object) (slot-def persistent-slot-definition))
  "Set the slot value in the database."
  (if (indexed class)
      (indexed-slot-writer class instance slot-def new-value)
      (let ((name (slot-definition-name slot-def)))
	(persistent-slot-writer (get-con instance) new-value instance name))))

(defmethod slot-boundp-using-class ((class persistent-metaclass) (instance persistent-object) (slot-def persistent-slot-definition))
  "Checks if the slot exists in the database."
  (when instance
    (let ((name (slot-definition-name slot-def)))
      (persistent-slot-boundp (get-con instance) instance name))))

(defmethod slot-boundp-using-class ((class persistent-metaclass) (instance persistent-object) (slot-name symbol))
  "Checks if the slot exists in the database."
  (loop for slot in (class-slots class)
     for matches-p = (eq (slot-definition-name slot) slot-name)
     until matches-p
     finally (return (if (and matches-p
			      (subtypep (type-of slot) 'persistent-slot-definition))
			 (persistent-slot-boundp (get-con instance) instance slot-name)
			 (call-next-method)))))

(defmethod slot-makunbound-using-class ((class persistent-metaclass) (instance persistent-object) (slot-def persistent-slot-definition))
  "Removes the slot value from the database."
  (if (indexed class)
      (indexed-slot-makunbound class instance slot-def)
      (persistent-slot-makunbound (get-con instance) instance (slot-definition-name slot-def))))

;; ===================================
;; Multi-store error checking
;; ===================================

(defun valid-persistent-reference-p (object sc)
  "Ensures that object can be written as a reference into store sc"
  (eq (dbcn-spc-pst object) (controller-spec sc)))

(define-condition cross-reference-error (error)
  ((object :accessor cross-reference-error-object :initarg :object)
   (home-controller :accessor cross-reference-error-home-controller :initarg :home-ctrl)
   (foreign-controller :accessor cross-reference-error-foreign-controller :initarg :foreign-ctrl))
  (:documentation "An error condition raised when an object is being written into a data store other
                   than its home store")
  (:report (lambda (condition stream)
	     (format stream "Attempted to write object ~A with home store ~A into store ~A"
		     (cross-reference-error-object condition)
		     (cross-reference-error-home-controller condition)
		     (cross-reference-error-foreign-controller condition)))))

(defun signal-cross-reference-error (object sc)
  (cerror "Proceed to write incorrect reference"
	  'cross-reference-error
	  :object object
	  :home-ctrl (get-con object)
	  :foreign-ctrl sc))

;; ======================================================
;; Handling metaclass overrides of normal slot operation
;; ======================================================

;;
;; ALLEGRO 
;;

#+allegro
(defmethod slot-makunbound-using-class ((class persistent-metaclass) (instance persistent-object) (slot-name symbol))
  (loop for slot in (class-slots class)
     until (eq (slot-definition-name slot) slot-name)
     finally (return (if (typep slot 'persistent-slot-definition)
			 (if (indexed class)
			     (indexed-slot-makunbound class instance slot)
			     (slot-makunbound-using-class class instance slot))
			 (call-next-method)))))


#+allegro
(defun make-persistent-reader (name slot-definition class class-name)
  (eval `(defmethod ,name ((instance ,class-name))
	  (slot-value-using-class ,class instance ,slot-definition))))

#+allegro
(defun make-persistent-writer (name slot-definition class class-name)
  (let ((name (if (and (consp name)
		       (eq (car name) 'setf))
		  name
		  `(setf ,name))))
    (eval `(defmethod ,name ((instance ,class-name) value)
	     (setf (slot-value-using-class ,class instance ,slot-definition)
		   value)))))

#+allegro
(defmethod initialize-accessors ((slot-definition persistent-slot-definition) class)
  (let ((readers (slot-definition-readers slot-definition))
	(writers (slot-definition-writers slot-definition))
	(class-name (class-name class)))
    (loop for reader in readers
	  do (make-persistent-reader reader slot-definition class class-name))
    (loop for writer in writers
	  do (make-persistent-writer writer slot-definition class class-name))))

;;
;; CMU / SBCL
;;

#+(or cmu sbcl)
(defun make-persistent-reader (name)
  (lambda (instance)
    (declare (type persistent-object instance))
    (persistent-slot-reader (get-con instance) instance name)))

#+(or cmu sbcl)
(defun make-persistent-writer (name)
  (lambda (new-value instance)
    (declare (optimize (speed 3))
	     (type persistent-object instance))
    (persistent-slot-writer (get-con instance) new-value instance name)))

#+(or cmu sbcl)
(defun make-persistent-slot-boundp (name)
  (lambda (instance)
    (declare (type persistent-object instance))
    (persistent-slot-boundp (get-con instance) instance name)))

#+sbcl ;; CMU also?  Old code follows...
(defmethod initialize-internal-slot-functions ((slot-def persistent-slot-definition))
  (let ((name (slot-definition-name slot-def)))
    (setf (slot-definition-reader-function slot-def)
	  (make-persistent-reader name))
    (setf (slot-definition-writer-function slot-def)
	  (make-persistent-writer name))
    (setf (slot-definition-boundp-function slot-def)
	  (make-persistent-slot-boundp name)))
  (call-next-method)) ;;  slot-def)

#+cmu
(defmethod initialize-internal-slot-functions ((slot-def persistent-slot-definition))
  (let ((name (slot-definition-name slot-def)))
    (setf (slot-definition-reader-function slot-def)
	  (make-persistent-reader name))
    (setf (slot-definition-writer-function slot-def)
	  (make-persistent-writer name))
    (setf (slot-definition-boundp-function slot-def)
	  (make-persistent-slot-boundp name)))
  slot-def)

;;
;; LISPWORKS
;;

#+lispworks
(defmethod slot-value-using-class ((class persistent-metaclass) (instance persistent-object) slot)
  (let ((slot-def (or (find slot (class-slots class) :key 'slot-definition-name)
		      (find slot (class-slots class)))))
    (if (typep slot-def 'persistent-slot-definition)
	(persistent-slot-reader (get-con instance) instance (slot-definition-name slot-def))
	(call-next-method class instance (slot-definition-name slot-def)))))

#+lispworks
(defmethod (setf slot-value-using-class) (new-value (class persistent-metaclass) (instance persistent-object) slot)
  "Set the slot value in the database."
  (let ((slot-def (or (find slot (class-slots class) :key 'slot-definition-name)
		      (find slot (class-slots class)))))
    (if (typep slot-def 'persistent-slot-definition)
	(if (indexed class)
	    (indexed-slot-writer class instance slot-def new-value)
	    (persistent-slot-writer (get-con instance) new-value instance (slot-definition-name slot-def)))
	(call-next-method new-value class instance (slot-definition-name slot-def)))))

#+lispworks
(defmethod slot-makunbound-using-class ((class persistent-metaclass) (instance persistent-object) slot)
  "Removes the slot value from the database."
  (let ((slot-def (or (find slot (class-slots class) :key 'slot-definition-name)
		      (find slot (class-slots class)))))
    (if (typep slot-def 'persistent-slot-definition)
	(if (indexed class)
	    (indexed-slot-makunbound class instance slot-def)
	    (persistent-slot-makunbound (get-con instance) instance (slot-definition-name slot-def)))
	(call-next-method class instance (slot-definition-name slot-def)))))