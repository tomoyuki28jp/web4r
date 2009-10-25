;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;
;;; metaclasses.lisp -- persistent objects via metaobjects
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
;;; Portions Copyright (c) 2005-2007 by Robert Read and Ian Eslick
;;; <rread common-lisp net> <ieslick common-lisp net>
;;;
;;; Elephant users are granted the rights to distribute and use this software
;;; as governed by the terms of the Lisp Lesser GNU Public License
;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;
(in-package "ELEPHANT")

(declaim #-elephant-without-optimize (optimize (speed 3) (safety 1)))

(defclass persistent ()
  ((%oid :accessor oid :initarg :from-oid
	 :documentation "All persistent objects have an oid")
   (dbconnection-spec-pst :type (or list string) :accessor dbcn-spc-pst :initarg :dbconnection-spec-pst
			  :documentation "Persistent objects use a spec pointer to identify which store
                                          they are connected to"))
  (:documentation "Abstract superclass for all persistent classes (common
    to both user-defined classes and Elephant-defined objects such as collections.)"))

(defmethod print-object ((obj persistent) stream)
  "This is useful for debugging and being clear about what is persistent and what is not"
  (format stream "#<~A oid:~A>" (type-of obj) (oid obj)))

(defclass persistent-metaclass (standard-class)
  ((%persistent-slots :accessor %persistent-slots)
   (%indexed-slots :accessor %indexed-slots)
   (%index-cache :accessor %index-cache))
  (:documentation 
   "Metaclass for persistent classes.  Use this metaclass to
    define persistent classes.  All slots are persistent by
    default; use the :transient flag otherwise.  Slots can also
    be indexed for by-value retrieval."))

;;
;; Top level defclass form - hide metaclass option
;;

(defmacro defpclass (cname parents slot-defs &rest class-opts)
  "Shorthand for defining persistent objects.  Wraps the main
   class definition with persistent-metaclass"
  `(defclass ,cname ,parents
     ,slot-defs
     ,@(add-persistent-metaclass-argument class-opts)))

(defun add-persistent-metaclass-argument (class-opts)
  (when (assoc :metaclass class-opts)
    (error "User metaclass specification not allowed in defpclass"))
  (append class-opts (list (list :metaclass 'persistent-metaclass))))
	  
;;
;; Persistent slot maintenance
;;

(defmethod persistent-slots ((class standard-class))
  nil)

(defmethod persistent-slots ((class persistent-metaclass))
  (if (slot-boundp class '%persistent-slots)
      (car (%persistent-slots class))
      nil))

(defmethod old-persistent-slots ((class persistent-metaclass))
  (cdr (%persistent-slots class)))

(defmethod update-persistent-slots ((class persistent-metaclass) new-slot-list)
  (setf (%persistent-slots class) 
	(cons new-slot-list 
	      (if (slot-boundp class '%persistent-slots)
		  (car (%persistent-slots class))
		  nil))))

(defclass persistent-slot-definition (standard-slot-definition)
  ((indexed :accessor indexed :initarg :index :initform nil :allocation :instance)))

(defclass persistent-direct-slot-definition (standard-direct-slot-definition persistent-slot-definition)
  ())

(defclass persistent-effective-slot-definition (standard-effective-slot-definition persistent-slot-definition)
  ())


(defclass transient-slot-definition (standard-slot-definition)
  ((transient :initform t :initarg :transient :allocation :class)))

(defclass transient-direct-slot-definition (standard-direct-slot-definition transient-slot-definition)
  ())

(defclass transient-effective-slot-definition (standard-effective-slot-definition transient-slot-definition)
  ())

(defgeneric transient (slot)
  (:method ((slot standard-direct-slot-definition)) t)
  (:method ((slot persistent-direct-slot-definition)) nil))

;;
;; Indexed slots maintenance
;;

;; This just encapsulates record keeping a bit
(defclass indexing-record ()
  ((class :accessor indexing-record-class :initarg :class :initform nil)
   (slots :accessor indexing-record-slots :initarg :slots :initform nil)
   (derived-count :accessor indexing-record-derived :initarg :derived :initform 0)))

(defmethod print-object ((obj indexing-record) stream)
  (format stream "#INDEXING-RECORD<islt: ~A dslt: ~A>" 
	  (length (indexing-record-slots obj))
	  (length (indexing-record-derived obj))))

(defmethod indexed-record ((class standard-class)) 
  nil)

(defmethod indexed-record ((class persistent-metaclass))
  (when (slot-boundp class '%indexed-slots)
    (car (%indexed-slots class))))

(defmethod old-indexed-record ((class persistent-metaclass))
  (when (slot-boundp class '%indexed-slots)
    (cdr (%indexed-slots class))))

(defmethod update-indexed-record ((class persistent-metaclass) new-slot-list &key class-indexed)
  (let ((oldrec (if (slot-boundp class '%indexed-slots)
		    (indexed-record class)
		    nil)))
    (setf (%indexed-slots class) 
	  (cons (make-new-indexed-record new-slot-list oldrec (or new-slot-list class-indexed))
		(if oldrec oldrec nil)))))

(defmethod make-new-indexed-record (new-slot-list oldrec class-indexed)
  (make-instance 'indexing-record 
		 :class (or class-indexed
			    (when oldrec (indexing-record-class oldrec)))
		 :slots new-slot-list
		 :derived (when oldrec (indexing-record-derived oldrec))))

(defmethod removed-indexing? ((class persistent-metaclass))
  (and (not (indexed class))
       (previously-indexed class)))

(defun indexed-slot-names-from-defs (class)
  (let ((slot-definitions (class-slots class)))
    (loop for slot-definition in slot-definitions
       when (and (subtypep (type-of slot-definition) 'persistent-slot-definition)
		 (indexed slot-definition))
       collect (slot-definition-name slot-definition))))

(defmethod register-indexed-slot ((class persistent-metaclass) slot)
  "This method allows for post-definition update of indexed status of
   class slots.  It changes the effective method so we can rely on 
   generic function dispatch for differentated behavior"
  ;; update record
  (let ((record (indexed-record class)))
    (unless (member slot (car (%persistent-slots class)))
      (error "Tried to register slot ~A as index which isn't a persistent slot" slot))
    (unless (member slot (indexing-record-slots record))
;;      This is a normal startup case, but during other cases we'd like
;;      the duplicate warning
;;      (warn "Tried to index slot ~A which is already indexed" slot))
      (push slot (indexing-record-slots record))))
  ;; change effective slot def
  (let ((slot-def (find-slot-def-by-name class slot)))
    (unless slot-def
      (error "Slot definition for slot ~A not found, inconsistent state in
              class ~A" slot (class-name class)))
    (setf (slot-value slot-def 'indexed) t)))

(defmethod unregister-indexed-slot (class slot)
  "Revert an indexed slot to it's original state"
  ;; update record
  (let ((record (indexed-record class)))
    (unless (member slot (indexing-record-slots record))
      (error "Tried to unregister slot ~A which is not indexed" slot))
    (setf (indexing-record-slots record) (remove slot (indexing-record-slots record))))
  ;; change effective slot def status
  (let ((slot-def (find-slot-def-by-name class slot)))
    (unless slot-def
      (error "Slot definition for slot ~A not found, inconsistent state in
              class ~A" slot (class-name class)))
    (setf (slot-value slot-def 'indexed) nil)))

(defmethod register-derived-index (class name)
  "Tell the class that it has derived indices defined against it
   and keep a reference count"
  (let ((record (indexed-record class)))
    (push name (indexing-record-derived record))))

(defmethod unregister-derived-index (class name)
  (let ((record (indexed-record class)))
    (setf (indexing-record-derived record) (remove name (indexing-record-derived record)))))

(defmethod indexed ((class persistent-metaclass))
  (and (slot-boundp class '%indexed-slots)
       (not (null (%indexed-slots class)))
       (or (indexing-record-class (indexed-record class))
	   (indexing-record-slots (indexed-record class))
	   (indexing-record-derived (indexed-record class)))))

(defmethod previously-indexed ((class persistent-metaclass))
  (and (slot-boundp class '%indexed-slots)
       (not (null (%indexed-slots class)))
       (let ((old (old-indexed-record class)))
	 (when (not (null old))
	   (or (indexing-record-class old)
	       (indexing-record-slots old)
	       (indexing-record-derived old))))))

(defmethod indexed ((slot standard-slot-definition)) nil)
(defmethod indexed ((class standard-class)) nil)

(defvar *inhibit-indexing-list* nil
  "Use this to avoid updating an index inside
   low-level functions that update groups of
   slots at once.  We may need to rethink this
   if we go to a cheaper form of update that
   doesn't batch update all indices")

(defun inhibit-indexing (uid)
  (pushnew uid *inhibit-indexing-list*))

(defun uninhibit-indexing (uid)
  (setf *inhibit-indexing-list*
	(delete uid *inhibit-indexing-list*)))

;;
;; Original support for persistent slot protocol
;;

#+allegro
(defmethod excl::valid-slot-allocation-list ((class persistent-metaclass))
  '(:instance :class :database))

(defmethod slot-definition-allocation ((slot-definition persistent-slot-definition))
  :database)

#+lispworks
(defmethod (setf slot-definition-allocation) (allocation (slot-def persistent-slot-definition))
  (unless (eq allocation :database)
    (error "Invalid allocation type ~A for slot-definition-allocation" allocation))
  allocation)

(defmethod direct-slot-definition-class ((class persistent-metaclass) &rest initargs)
  "Checks for the transient tag (and the allocation type)
   and chooses persistent or transient slot definitions."
  (let ((allocation-key (getf initargs :allocation))
	(transient-p (getf initargs :transient))
	(indexed-p (getf initargs :index)))
    (when (consp transient-p) (setq transient-p (car transient-p)))
    (when (consp indexed-p) (setq indexed-p (car indexed-p)))
    (cond ((and (eq allocation-key :class) transient-p)
	   (find-class 'transient-direct-slot-definition))
	  ((and (eq allocation-key :class) (not transient-p))
	   (error "Persistent class slots are not supported, try :transient t."))
	  ((and indexed-p transient-p)
	   (error "Cannot declare slots to be both transient and indexed"))
	  (transient-p
	   (find-class 'transient-direct-slot-definition))
	  (t
	   (find-class 'persistent-direct-slot-definition)))))

(defmethod validate-superclass ((class persistent-metaclass) (super standard-class))
  "Persistent classes may inherit from ordinary classes."
  t)

(defmethod validate-superclass ((class standard-class) (super persistent-metaclass))
  "Ordinary classes may NOT inherit from persistent classes."
  nil)

(defgeneric persistent-p (class)
  (:method ((class t)) nil)
  (:method ((class persistent-metaclass)) t)
  (:method ((class persistent-slot-definition)) t))

(defmethod effective-slot-definition-class ((class persistent-metaclass) &rest initargs)
  "Chooses the persistent or transient effective slot
definition class depending on the keyword."
  (let ((transient-p (getf initargs :transient))
	(indexed-p (getf initargs :index)))
    (when (consp transient-p) (setq transient-p (car transient-p)))
    (when (consp indexed-p) (setq indexed-p (car indexed-p)))
    (cond ((and indexed-p transient-p)
	   (error "Cannot declare a slot to be both indexed and transient"))
	  (transient-p
	   (find-class 'transient-effective-slot-definition))
	  (t
	   (find-class 'persistent-effective-slot-definition)))))

#+openmcl
(defmethod compute-effective-slot-definition ((class persistent-metaclass) slot-name direct-slot-definitions)
  (declare (ignore slot-name))
  (apply #'make-effective-slot-definition class
	 (compute-effective-slot-definition-initargs 
	 class direct-slot-definitions)))

#+openmcl
(defmethod compute-effective-slot-definition-initargs ((class slots-class)
						       direct-slots)
  (let* ((name (loop for s in direct-slots
		  when s
		  do (return (slot-definition-name s))))
	 (initer (dolist (s direct-slots)
                   (when (%slot-definition-initfunction s)
                     (return s))))
         (documentor (dolist (s direct-slots)
                       (when (%slot-definition-documentation s)
                         (return s))))
         (first (car direct-slots))
         (initargs (let* ((initargs nil))
                     (dolist (dslot direct-slots initargs)
                       (dolist (dslot-arg (%slot-definition-initargs  dslot))
                         (pushnew dslot-arg initargs :test #'eq))))))
    (list
     :name name
     :allocation (%slot-definition-allocation first)
     :documentation (when documentor (nth-value
                                      1
                                      (%slot-definition-documentation
                                       documentor)))
     :class (%slot-definition-class first)
     :initargs initargs
     :initfunction (if initer (%slot-definition-initfunction initer))
     :initform (if initer (%slot-definition-initform initer))
     :type (or (%slot-definition-type first) t))))

(defun ensure-transient-chain (slot-definitions initargs)
  (declare (ignore initargs))
  (loop for slot-definition in slot-definitions
     always (transient slot-definition)))

(defmethod compute-effective-slot-definition-initargs ((class persistent-metaclass) #+lispworks slot-name slot-definitions)
  #+lispworks (declare (ignore slot-name))
  (let ((initargs (call-next-method)))
    (if (ensure-transient-chain slot-definitions initargs)
	(setf initargs (append initargs '(:transient t)))
	(setf (getf initargs :allocation) :database))
    ;; Effective slots are indexed only if the most recent slot definition
    ;; is indexed.  NOTE: Need to think more about inherited indexed slots
    (if (indexed (first slot-definitions))
	(append initargs '(:index t))
	initargs)))

(defun find-slot-def-by-name (class slot-name)
  (loop for slot-def in (class-slots class)
	when (eq (slot-definition-name slot-def) slot-name)
	do (return slot-def)))


(defun persistent-slot-defs (class)
  (let ((slot-definitions (class-slots class)))
    (loop for slot-def in slot-definitions
	 when (subtypep (type-of slot-def) 'persistent-effective-slot-definition)
	 collect slot-def)))

(defun transient-slot-defs (class)
  (let ((slot-definitions (class-slots class)))
    (loop for slot-def in slot-definitions
       unless (persistent-p slot-def)
       collect slot-def)))

(defun persistent-slot-names (class)
  (mapcar #'slot-definition-name (persistent-slot-defs class)))

(defun transient-slot-names (class)
  (mapcar #'slot-definition-name (transient-slot-defs class)))



