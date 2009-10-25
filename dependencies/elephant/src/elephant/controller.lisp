;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
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

(in-package "ELEPHANT")

;;
;; TRACKING OBJECT STORES
;;

(defvar *elephant-data-stores*
  '((:bdb (:ele-bdb))
    (:clsql (:ele-clsql))
    )
  "Tells the main elephant code the tag used in a store spec to
   refer to a given data store.  The second argument is an asdf
   dependency list.  Entries have the form of 
  (data-store-type-tag asdf-depends-list")

(defvar *elephant-controller-init* (make-hash-table))

(defun register-data-store-con-init (name controller-init-fn)
  "Data stores must call this function during the
   loading/compilation process to register their initialization
   function for the tag name in *elephant-data-stores*.  The
   initialization function returns a fresh instance of the
   data stores store-controller subclass"
  (setf (gethash name *elephant-controller-init*) controller-init-fn))

(defun lookup-data-store-con-init (name)
  (gethash name *elephant-controller-init*))

(defvar *dbconnection-spec* (make-hash-table :test 'equal))
(defvar *dbconnection-lock* (ele-make-lock))

(defgeneric get-con (instance &optional sc)
  (:documentation "This is used to find and validate the connection spec
   maintained for in-memory persistent objects.  Should
   we re-open the controller from the spec if it's not
   cached?  That might be dangerous so for now we error"))

(define-condition controller-lost-error ()
   ((object :initarg :object :accessor store-controller-closed-error-object)
    (spec :initarg :spec :accessor store-controller-closed-error-spec)))

(defun signal-controller-lost-error (object)
  (cerror "Open a new instance and continue?"
	  'controller-lost-error
	  :format-control "Store controller for specification ~A for object ~A cannot be found."
	  :format-arguments (list object (dbcn-spc-pst object))
	  :object object
	  :spec (dbcn-spc-pst object)))

(defmethod get-con ((instance persistent) &optional (sc *store-controller*))
  (declare (ignore sc))
  (let ((con (gethash (dbcn-spc-pst instance) *dbconnection-spec*)))
    (cond ((not con)
	   (progn (signal-controller-lost-error instance)
		  (open-controller 
		   (get-controller (dbcn-spc-pst instance)))))
	  ;; If it's valid and open
	  ((and con (connection-is-indeed-open con))
	   con)
	  ;; If the controller object exists but is closed, reopen
	  (t (open-controller con)))
    con))

(defun get-controller (spec)
  "This is used by open-store to fetch or open a controller.
   This maintains the dbconnection-spec table so should be
   the only point of entry for getting access to controllers
   from specs.  Get-con is used to validate connections and
   reopen if necessary and perhaps these two should be combined
   at some point"
  (let ((cached-sc (gethash spec *dbconnection-spec*)))
    (if (and cached-sc (connection-is-indeed-open cached-sc))
	cached-sc
	(build-controller spec))))

(defun build-controller (spec)
  "Actually construct the controller & load dependencies"
  (assert (and (consp spec) (symbolp (first spec))))
  (load-data-store (first spec))
  (let ((init (lookup-data-store-con-init (first spec))))
      (unless init (error "Store controller init function not registered for data store ~A." (car spec)))
      (let ((sc (funcall (symbol-function init) spec)))
	(ele-with-lock (*dbconnection-lock*)
	  (setf (gethash spec *dbconnection-spec*) sc))
	sc)))

(defun load-data-store (type)
  (assert (find-package :asdf))
  (let ((record (assoc type *elephant-data-stores*)))
    (when (or (null record) (not (consp record)))
      (error "Unknown data store type ~A, cannot load" type))
    (satisfy-asdf-dependencies (second record))))

(defun satisfy-asdf-dependencies (dep-list)
  (mapc #'(lambda (dep) 
	    ;; Only load the first time, after that it's the 
	    ;; users fault if they edit source code
	    (unless (asdf::system-registered-p dep)
	      (asdf:operate 'asdf:load-op dep)))
	dep-list))

;;
;; PER-USER INSTALLATION PARAMETERS
;;

(defun get-user-configuration-parameter (name)
  "This function pulls a value from the key-value pairs stored in
   my-config.sexp so data stores can have their own pairs for appropriate
   customization after loading."
  (elephant-system::get-config-option
   name
   (asdf:find-system :elephant)))

(defun initialize-user-parameters ()
  (loop for (keyword variable) in *user-configurable-parameters* do
       (awhen (get-user-configuration-parameter keyword)
	 (setq variable it))))

;;
;; COMMON STORE CONTROLLER FUNCTIONALITY
;;

(defclass store-controller ()
  ((spec :type list
	 :accessor controller-spec
	 :initarg :spec
	 :documentation "Data store initialization functions are
	 expected to initialize :spec on the call to
	 make-instance")
   ;; Generic support for the object, indexing and root protocols
   (root :reader controller-root 
	 :documentation "This is an instance of the data store
	 persistent btree.  It should have an OID that is fixed in
	 the code and does not change between sessions.  Usually
	 it this is something like 0, 1 or -1")
   (class-root :reader controller-class-root
	       :documentation 
	       "This is another root for class indexing that is
	       also a data store specific persistent btree instance
	       with a unique OID that persists between sessions.")
   (instance-cache :accessor instance-cache :initform (make-cache-table :test 'eql)
		   :documentation 
		   "This is an instance cache and part of the
                    metaclass protocol.  Data stores should not
                    override the default behavior.")
   (instance-cache-lock :accessor instance-cache-lock :initform (ele-make-fast-lock)
			:documentation "Protection for updates to
			the cache from multiple threads.  Do not
			override.")
   ;; Upgradable serializer strategy
   (serializer-version :accessor controller-serializer-version :initform nil
		       :documentation "Governs the default
		       behavior regarding which serializer
		       version the current elephant core is
		       using.  Data stores can override by creating
		       a method on initialize-serializer.")
   (serialize :accessor controller-serialize :initform nil
	      :documentation "Accessed by elephant::serialize to
	      get the entry point to the default serializer or to
	      a data store specific serializer")
   (deserialize :accessor controller-deserialize :initform nil
		:documentation "Contains the entry point for the
		specific serializer to be called by
		elephant::deserialize"))
  (:documentation 
   "Superclass for the data store controller, the main interface
    to any book-keeping, references to DB handles, the instance
    cache, btree table creation, counters, locks, the roots (for
    garbage collection,) et cetera.  Behavior is shared between
    the superclass and subclasses.  See slot documentation for
    details."))

(defmethod print-object ((sc store-controller) stream)
  (format stream "#<~A ~A>" (type-of sc) (second (controller-spec sc))))

;;
;; Per-controller instance caching
;;

(defun cache-instance (sc obj)
  "Cache a persistent object with the controller."
  (declare (type store-controller sc))
  (ele-with-fast-lock ((instance-cache-lock sc))
    (setf (get-cache (oid obj) (instance-cache sc)) obj)))

(defun get-cached-instance (sc oid class-name)
  "Get a cached instance, or instantiate!"
  (declare (type store-controller sc)
	   (type fixnum oid))
  (let ((obj 
	 (ele-with-fast-lock ((instance-cache-lock sc))
	   (get-cache oid (instance-cache sc)))))
    (if obj obj
	;; Should get cached since make-instance calls cache-instance
	(make-instance class-name :from-oid oid :sc sc))))

(defmethod flush-instance-cache ((sc store-controller))
  "Reset the instance cache (flush object lookups).  Useful 
   for testing.  Does not reclaim existing objects so there
   will be duplicate instances with identical functionality"
  (ele-with-fast-lock ((instance-cache-lock sc))
    (setf (instance-cache sc)
	  (make-cache-table :test 'eql))))


;;
;; Database versioning
;;

(defgeneric database-version (sc)
  (:documentation "Data stores implement this to store the serializer version.
                   The protocol requires that data stores report their database
                   version.  On new database creation, the database is written with the
                   *elephant-code-version* so that is returned by database-version.
                   If a legacy database does not have a version according to the method
                   then it should return nil"))

(defmethod database-version :around (sc)
  "Default version assumption for unmarked databases is 0.6.0.
   It is possible to check for 0.5.0 databases, but it is not implemented 
   now due to the low (none?) number of users still on 0.5.0"
  (declare (ignorable sc))
  (let ((db-version (call-next-method)))
    (if db-version db-version
	'(0 6 0))))

(defun prior-version-p (v1 v2)
  "Is v1 an equal or earlier version than v2"
  (cond ((and (null v1) (null v2))         t)
        ((and (null v1) (not (null v2)))   t)
	((and (not (null v1)) (null v2))   nil)
	((< (car v1) (car v2))             t)
	((> (car v1) (car v2))             nil)
	((= (car v1) (car v2))
	 (prior-version-p (cdr v1) (cdr v2)))
	(t (error "Version comparison problem: (prior-version-p ~A ~A)" v1 v2))))

;;
;; Database upgrade paths
;;

(defparameter *elephant-upgrade-table*
  '( ((0 6 0) (0 5 0))
     ((0 9 0) (0 6 0))
   ))

(defmethod up-to-date-p ((sc store-controller))
  (equal (database-version sc) *elephant-code-version*))

(defmethod upgradable-p ((sc store-controller))
  "Determine if this store can be brought up to date using the upgrade function"
  (unwind-protect
       (let ((row (assoc *elephant-code-version* *elephant-upgrade-table* :test #'equal))
	     (ver (database-version sc)))
	 (when (member ver (rest row) :test #'equal)) t)
    nil))

(defgeneric upgrade (sc spec)
  (:documentation "Given an open store controller from a prior version, 
                   open a new store specified by spec and migrate the
                   data from the original store to the new one, upgrading
                   it to the latest version"))

(defmethod upgrade ((sc store-controller) target-spec)
  (unless (upgradable-p sc)
    (error "Cannot upgrade ~A from version ~A to version ~A~%Valid upgrades are:~%~A" 
	   (controller-spec sc)
	   (database-version sc)
	   *elephant-code-version*
	   *elephant-upgrade-table*))
  (warn "Please read the current limitations on migrate-based upgrade in migrate.lisp to ensure your 
         data does not require any unsupported features")
  (let ((source sc)
	(target (open-store target-spec)))
    (migrate target source)
    (close-store target)))

;;
;; Modular serializer support and default serializers for a version
;;

(defmethod initialize-serializer ((sc store-controller))
  "Establish serializer version on controller startup.  Data stores call this before
   they need the serializer to be valid and after they enable their database-version
   call.  If the data store shadows this, it has to keep track of serializer versions 
   associated with the database version that is opened."
  (cond ((prior-version-p (database-version sc) '(0 6 0))
	 (setf (controller-serializer-version sc) 1)
	 (setf (controller-serialize sc) 
	       (intern "SERIALIZE" (find-package :ELEPHANT-SERIALIZER1)))
	 (setf (controller-deserialize sc)
	       (intern "DESERIALIZE" (find-package :ELEPHANT-SERIALIZER1))))
	(t 
	 (setf (controller-serializer-version sc) 2)
	 (setf (controller-serialize sc) 
	       (intern "SERIALIZE" (find-package :ELEPHANT-SERIALIZER2)))
	 (setf (controller-deserialize sc) 
	       (intern "DESERIALIZE" (find-package :ELEPHANT-SERIALIZER2))))))

;;
;; Handling package changes in legacy databases 
;;

(defvar *always-convert* nil)

(defparameter *legacy-symbol-conversions*
  '(;; 0.5.0 support 
    (("elephant" . "bdb-btree") . ("sleepycat" . "bdb-btree"))
    (("elephant" . "bdb-indexed-btree") . ("sleepycat" . "bdb-indexed-btree"))
    (("elephant" . "bdb-btree-index") . ("sleepycat" . "bdb-btree-index"))))

(defun add-symbol-conversion (old-name old-package new-name new-package old-version)
  "Users can specify specific symbol conversions on upgrade prior to 
   migrating old databases"
  (declare (ignore old-version))
  (push (cons (cons old-name old-package) (cons new-name new-package)) *legacy-symbol-conversions*))

(defun map-legacy-symbols (symbol-string package-string old-version)
  (declare (ignore old-version))
  (let ((entry (assoc (cons (string-upcase symbol-string) (string-upcase package-string))
		      *legacy-symbol-conversions* :test #'equal)))
    (if entry
	(values t (cadr entry) (cddr entry))
	nil)))


(defparameter *legacy-package-conversions*
  '(("ELEPHANT-CLSQL" . "DB-CLSQL")
    ("SLEEPYCAT" . "DB-BDB")))

(defun add-package-conversion (old-package-string new-package-string old-version)
  "Users can specify wholesale package name conversions on upgrade 
   prior to migrating old databases"
  (declare (ignore old-version))
  (push (cons old-package-string new-package-string) *legacy-package-conversions*))

(defun map-legacy-package-names (package-string old-version)
  (declare (ignore old-version))
  (let ((entry (assoc (string-upcase package-string) *legacy-package-conversions* :test #'equal)))
    (if entry
	(cdr entry)
	package-string)))

(defun map-legacy-names (symbol-name package-name old-version)
  (multiple-value-bind (mapped? new-name new-package)
      (map-legacy-symbols symbol-name package-name old-version)
    (if mapped?
	(values new-name new-package)
	(values new-name (map-legacy-package-names package-name old-version)))))

(defun translate-and-intern-symbol (symbol-name package-name db-version)
  "Service for the serializer to translate any renamed packages or symbols
   and then intern the decoded symbol."
  (if package-name 
      (multiple-value-bind (sname pname)
	  (if (or *always-convert* (not (equal db-version *elephant-code-version*)))
	      (map-legacy-names symbol-name package-name db-version)
	      (values symbol-name package-name))
	(let ((package (find-package pname)))
	  (if package
	      (intern sname package)
	      (progn
		(warn "Couldn't deserialize the package: ~A based on ~A~%
                       An uninterred symbol will be created" pname package-name)
		(make-symbol sname)))))
      (make-symbol symbol-name)))

;; ================================================================================
;;
;;                  DATA STORE CONTROLLER PROTOCOL
;;
;; ================================================================================

(defgeneric open-controller (sc &key recover recover-fatal thread &allow-other-keys)
  (:documentation 
   "Opens the underlying environment and all the necessary
database tables.  Different data stores may use different keys so
all methods should &allow-other-keys.  There are three standard
keywords: :recover, :recover-fatal and :thread.  Recover means
that recovery should be checked for or performed on startup.
Recover fatal means a full rebuild from log files is requested.
Thread merely indicates to the data store that it is a threaded
application and any steps that need to be taken (for example
transaction implementation) are taken.  :thread is usually
true."))

(defgeneric close-controller (sc)
  (:documentation 
   "Close the db handles and environment.  Should be in a state
   where lisp could be shut down without causing an inconsistent
   state in the db.  Also, the object could be used by
   open-controller to reopen the database"))

(defmethod close-controller :after ((sc store-controller))
  "Delete connection spec so store-controller operations on cached 
   controller information fail"
  (remhash (controller-spec sc) *dbconnection-spec*))

(defgeneric connection-is-indeed-open (controller)
  (:documentation "Validate the controller and the db that it is connected to")
  (:method ((controller t)) t))

(defgeneric next-oid (sc)
  (:documentation
   "Provides a persistent source of unique id's"))

(defgeneric optimize-layout (sc &key &allow-other-keys)
  (:documentation "If supported, speed up the index and allocation by freeing up
                   any available storage and return it to the free list.  See the
                   methods of data stores to determine what options are valid. Supported
                   both on stores (all btrees and persistent slots) and specific btrees"))

;;
;; Low-level support for metaclass protocol 
;;

(defgeneric persistent-slot-reader (sc instance name)
  (:documentation 
   "Data store specific slot reader function"))

(defgeneric persistent-slot-writer (sc new-value instance name)
  (:documentation 
   "Data store specific slot writer function"))

(defgeneric persistent-slot-boundp (sc instance name)
  (:documentation
   "Data store specific slot bound test function"))

(defgeneric persistent-slot-makunbound (sc instance name)
  (:documentation
   "Data store specific slot makunbound handler"))


;; ================================================================================
;;
;;                             CONTROLLER USER API
;;   
;; ================================================================================


;;
;; Opening and closing data stores
;;

(defun open-store (spec &rest args)
  "Conveniently open a store controller.  Set *store-controller* to the new controller
   unless it is already set (opening a second controller means you must keep track of
   controllers yourself.  *store-controller* is a convenience variable for single-store
   applications or single-store per thread apps.  Multi-store apps should either confine
   their *store-controller* to a given dynamic context or wrap each store-specific op in
   a transaction using with or ensure transaction"
  (assert (consp spec))
  ;; Ensure that parameters are set
  (initialize-user-parameters)
  (let ((controller (get-controller spec)))
    (apply #'open-controller controller args)
    (if *store-controller*
	(progn
;;	  (warn "Store controller already set so was not updated") ;; this was annoying me
	  controller)
	(setq *store-controller* controller))))

(defun close-store (&optional sc)
  "Conveniently close the store controller."
  (when (or sc *store-controller*)
    (close-controller (or sc *store-controller*)))
  (unless sc
    (setf *store-controller* nil)))

(defmacro with-open-store ((spec) &body body)
  "Executes the body with an open controller,
   unconditionally closing the controller on exit."
  `(let ((*store-controller* nil))
     (declare (special *store-controller*))
     (open-store ,spec)
     (unwind-protect
	  (progn ,@body)
       (close-store *store-controller*))))


;;
; Operations on the root index
;;

(defun add-to-root (key value &key (sc *store-controller*))
  "Add an arbitrary persistent thing to the root, so you can
   retrieve it in a later session.  Anything referenced by an
   object added to the root is considered reachable and thus live"
  (declare (type store-controller store-controller))
  (assert (not (eq key *elephant-properties-label*)))
  (setf (get-value key (controller-root sc)) value))

(defun get-from-root (key &key (sc *store-controller*))
  "Get the value associated with key from the root.  Returns two
   values, the value, or nil, and a boolean indicating whether a
   value was found or not (so you know if nil is a value or an
   indication of non-presence)"
  (declare (type store-controller sc))
  (get-value key (controller-root sc)))

(defun root-existsp (key &key (sc *store-controller*))
  "Test whether a given key is instantiated in the root"
  (declare (type store-controller sc))
  (if (existsp key (controller-root sc))
      t 
      nil))

(defun remove-from-root (key &key (sc *store-controller*))
  "Remove something from the root by the key value"
  (declare (type store-controller sc))
  (remove-kv key (controller-root sc)))

(defun map-root (fn &key (sc *store-controller*))
  "Takes a function of two arguments, key and value, to map over
   all key-value pairs in the root"
  (map-btree fn (controller-root sc)))

;;
;; Explicit storage reclamation
;;

(defgeneric drop-pobject (persistent-object)
  (:documentation   "drop-pobject reclaims persistent object storage by unbinding
   all persistent slot values.  It can also helps catch errors
   where an object should be unreachable, but a reference still
   exists elsewhere in the DB.  On access, the unbound slots
   should flag an error in the application program.  IMPORTANT:
   this function does not clear the cached object instance or any
   serialized references still in the db.  Need a migration or GC
   for that!  drop-instances is preferred as it implements the proper
   behavior for indexed classes"))

(defmethod drop-pobject ((inst persistent-object))
  (let ((pslots (persistent-slots (class-of inst))))
    (dolist (slot pslots)
      (slot-makunbound inst slot))))
;;      (slot-makunbound-using-class (class-of inst)
;;				   inst
;;				   (find-effective-slot-def (class-of inst) slot)))))

;;
;; DATABASE PROPERTY INTERFACE (Not used by system as of 0.6.1, but supported)
;;

(defvar *restricted-properties* '()
  "Properties that are not user manipulable")

(defmethod controller-properties ((sc store-controller))
  (get-from-root *elephant-properties-label* :store-controller sc))

(defmethod set-ele-property (property value &key (sc *store-controller*))
  (assert (and (symbolp property) (not (member property *restricted-properties*))))
  (let ((props (get-from-root *elephant-properties-label* :store-controller sc)))
    (setf (get-value *elephant-properties-label* (controller-root sc))
	  (if (assoc property props)
	      (progn (setf (cdr (assoc property props)) value)
		     props)
	      (acons property value props)))))

(defmethod get-ele-property (property &key (sc *store-controller*))
  (assert (symbolp property))
  (let ((entry (assoc property 
		      (get-from-root *elephant-properties-label* 
				     :store-controller sc))))
    (when entry
      (cdr entry))))

   
      

