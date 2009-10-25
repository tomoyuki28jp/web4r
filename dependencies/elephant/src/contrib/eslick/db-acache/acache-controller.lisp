
(in-package :elephant-acache)

(defclass acache-store-controller (store-controller)
  ((db :accessor controller-db :initform nil)
   (slots :accessor controller-slots :initform nil)
   (btrees :accessor controller-btrees :initform nil)
   (oidrec :accessor controller-oidrec :initform nil)))

(defun acache-constructor (spec)
  (make-instance 'acache-store-controller :spec spec))

(eval-when (:compile-toplevel :load-toplevel)
  (register-backend-con-init :acache 'acache-constructor))

(defclass oid-record ()
  ((counter :accessor oid-record-counter :initform 0))
  (:metaclass db.allegrocache:persistent-class))

(defmethod open-controller ((sc acache-store-controller) &key (recover t)
			    (recover-fatal nil) (thread nil))
  (declare (ignore recover thread recover-fatal))
  (let ((db (db.allegrocache:open-file-database (second (controller-spec sc))
						 :if-does-not-exist :create
						 :if-exists :open
						 :use :memory)))
    (when (not db)
      (error "Unable to open astore database for ~A" (controller-spec sc)))
    ;; Main DB ref
    (setf (controller-db sc) db)
    ;; Slots and Btree storage
    (let ((slotmap (retrieve-from-index 'ac-map 'ac-map-name "slots")))
      (setf (controller-slots sc) 
	    (if slotmap slotmap
		(make-instance 'db.allegrocache:ac-map :ac-map-name "slots"))))
    (let ((btreemap (retrieve-from-index 'ac-map 'ac-map-name "btrees")))
      (setf (controller-btrees sc) 
	    (if btreemap btreemap
		(make-instance 'db.allegrocache:ac-map :ac-map-name "btrees"))))
    ;; OIDS
    (let ((oidrec (doclass (inst (find-class 'oid-record) :db db)
		    (when inst (return inst)))))
      (setf (controller-oidrec sc)
	    (if oidrec
		oidrec
		(make-instance 'oid-record))))
    ;; Construct the roots
    (setf (slot-value sc 'root) (make-instance 'acache-btree :from-oid -1))
    (setf (slot-value sc 'class-root) (make-instance 'acache-btree :from-oid -2))
    sc))
    
    
(defmethod next-oid ((sc acache-store-controller))
  (incf (oid-record-counter (controller-oidrec sc))))

(defmethod close-controller ((sc acache-store-controller))
  ;; Ensure deletion of common
  (setf (slot-value sc 'class-root) nil)
  (setf (slot-value sc 'root) nil)
  (db.allegrocache:close-database :db (controller-db sc)))

(defmethod connection-is-indeed-open ((sc acache-store-controller))
  (db.allegrocache::database-open-p (controller-db sc)))

;; Slot writing

;; This is not thread-safe, but could be a thread-local when we fix that...
;; to avoid extra consing.  Is consing less/more expensive than dynamic
;; var lookups?

(defvar *index-cons* (cons nil nil))

(defmacro fast-key (oid name)
  `(rplacd (rplaca *index-cons* ,oid) ,name))

(defmethod persistent-slot-reader ((sc acache-store-controller) instance name)
  (declare (optimize (speed 3) (safety 1)))
  (multiple-value-bind (val valid?) (map-value (controller-slots sc) (fast-key (oid instance) name))
    (if valid?
	val
	(error "Slot ~A unbound in ~A" name instance))))

(defmethod persistent-slot-writer ((sc acache-store-controller) value instance name)
  (declare (optimize (speed 3) (safety 1)))
  (setf (map-value (controller-slots sc) (fast-key (oid instance) name))
	value))

(defmethod persistent-slot-boundp ((sc acache-store-controller) instance name)
  (declare (optimize (speed 3) (safety 1)))
  (when (map-value (controller-slots sc) (fast-key (oid instance) name))
    t))

(defmethod persistent-slot-makunbound ((sc acache-store-controller) instance name)
  (declare (optimize (speed 3) (safety 1)))
  (remove-from-map (controller-slots sc) (fast-key (oid instance) name)))

