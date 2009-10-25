

(in-package :elephant-acache)

;; BTREE

(defclass acache-btree (btree) ())

(defmethod build-btree ((sc acache-store-controller))
  (make-instance 'acache-btree :sc sc))

(defmethod get-value (key (bt acache-btree))
  (map-value (controller-btrees (get-con bt)) (cons (oid bt) key)))

(defmethod (setf get-value) (value key (bt acache-btree))
  (setf (map-value (controller-btrees (get-con bt)) (cons (oid bt) key))
	value))

(defmethod existsp (key (bt acache-btree))
  (when (get-value key bt)
    t))

(defmethod remove-kv (key (bt acache-btree))
  (remove-from-map (controller-btrees (get-con bt)) (cons (oid bt) key)))

(defmethod map-btree (fn (bt acache-btree))
  (map-map fn bt))



;; INDEXED BTREE

(defclass acache-indexed-btree (indexed-btree acache-btree)
  ((indices :accessor indices :initarg :indices :initform (make-hash-table))
   (indices-cache :accessor indices-cache :initarg :indicies-cache :initform nil :transient t))
  (:metaclass persistent-metaclass))

(defmethod build-indexed-btree ((sc acache-store-controller))
  (make-instance 'acache-indexed-btree :sc sc))

(defclass acache-btree-index (btree-index acache-btree) 
  ()
  (:metaclass persistent-metaclass))

(defmethod build-btree-index ((sc acache-store-controller) &key primary key-form)
  (make-instance 'acache-btree-index :primary primary :key-form :sc sc))

;;
;; CURSORS
;;

(defclass acache-cursor (cursor)
  ())

(defmethod make-cursor ((bt acache-btree))
  (make-instance 'acache-cursor))

