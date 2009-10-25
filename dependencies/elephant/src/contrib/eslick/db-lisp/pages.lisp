;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;
;;; ele-lisp.asd -- ASDF system definition for elephant lisp backend
;;; 
;;; part of
;;;
;;; Elephant Object Oriented Database: Common Lisp Backend
;;;
;;; Copyright (c) 2007 by Ian Eslick
;;; <ieslick at common-lisp.net>
;;;
;;; Elephant Lisp Backend users are granted the rights to distribute
;;; and use this software as governed by the terms of the Lisp Lesser
;;; GNU Public License (http://opensource.franz.com/preamble.html),
;;; also known as the LLGPL.

(in-package :db-lisp)

;;
;; Utilities
;;

(defun copy-slots (obj1 obj2 slotnames)
  (loop for slotname in slotnames do
       (setf (slot-value obj2 slotname) (slot-value obj1 slotname))))

(defun write-integer-to-array (integer array offset &optional (bytes 4))
  (declare (type fixnum offset bytes)
	   (type integer integer)
	   (type (array (unsigned-byte 8)) array))
  (assert (< offset (length array)))
  (loop for i fixnum from 0 below bytes do
       (setf (aref array (+ offset i))
	     (ldb (byte 8 (* i 8)) integer)))
  integer)

(defun read-integer-from-array (array offset &optional (bytes 4))
  (declare (type fixnum offset bytes)
	   (type (array (unsigned-byte 8)) array))
  (assert (< offset (length array)))
  (let ((value 0))
    (loop for i fixnum from 0 below bytes do
	 (setf value (dpb (aref array (+ i offset)) (byte 8 (* i 8)) value)))
    value))


;;
;; Mixins
;;

(defclass doubly-linked-list-mixin ()
  ((next :accessor dlist-next :initform nil)
   (prev :accessor dlist-prev :initform nil)))

(defmethod link-node ((node doubly-linked-list-mixin) before after)
  "Insert page into doubly linked list"
  (unless (null before)
    (setf (dlist-next before) node))
  (setf (dlist-next node) after)
  (setf (dlist-prev node) before)
  (unless (null after)
    (setf (dlist-prev after) node))
  node)

(defmethod unlink-node ((node doubly-linked-list-mixin))
  "Remove page from linked list; return next"
  (unless (null (dlist-next node))
    (setf (dlist-prev (dlist-next node)) (dlist-prev node)))
  (unless (null (dlist-prev node))
    (setf (dlist-next (dlist-prev node)) (dlist-next node)))
  node)



;; ============================================================================
;;
;; Buffer-Page -- Maintains a page of binary data
;;
;; ============================================================================

(defclass buffer-page (doubly-linked-list-mixin)
  ((position :accessor page-position :initarg :position :initform -1) ;; position
   (type :accessor page-type :initarg :type :initform :unknown)
   (size :accessor page-size :initarg :page-size :initform 4096)
   (dirty-p :accessor page-dirty-p :initform nil)
   (buffer :accessor page-buffer :type (simple-array (unsigned-byte 8) (*)))
   (stream :accessor page-stream-store))
  (:documentation "A buffer-page is an in-memory buffer containing the contents
   of a random access stream (usually a file)."))

(defmethod initialize-instance :after ((page buffer-page) &rest initargs)
  (declare (ignore initargs))
  (setf (page-buffer page) (make-array (page-size page)
				       :element-type '(unsigned-byte 8))))

;;
;; Primitive read-write of buffer-pages
;;

;; Read/Write fixnums

(defmethod write-integer (fixnum page offset &optional (bytes 4))
  (declare (type fixnum fixnum offset bytes))
  (write-integer-to-array fixnum (page-buffer page) offset bytes))

(defmethod read-integer (page offset &optional (bytes 4))
  (declare (type fixnum offset bytes))
  (read-integer-from-array (page-buffer page) offset bytes))

;; NOTE: Redo memutil/serializer primitives here?

(defmethod copy-page ((page1 buffer-page) (page2 buffer-page))
  (copy-slots page1 page2 '(position type size dirty-p stream))
  (loop for i fixnum from 0 below (page-size page2) do
        (setf (aref (page-buffer page2) i)
	      (aref (page-buffer page1) i))))

(defmethod copy-region ((page buffer-page) start length offset)
  "Move region defined by start and length offset bytes.  If offset
   is negative, move to lower parts of the array, if position, toward
   the end."
  (let ((buffer (page-buffer page)))
    (declare (type (array (unsigned-byte 8)) buffer))
    (if (< 0 offset)
	(loop for i from 0 below length do
	     (setf (aref buffer (+ start offset i))
		   (aref buffer (+ start i))))
	(loop for i from 0 below length do
	     (setf (aref buffer (- (+ start length offset) i))
		   (aref buffer (- (+ start length) i)))))))

;;
;; Read-write buffer-pages from buffer-streams
;;

(defmethod write-buffer-stream ((page buffer-page) (bs buffer-stream) offset)
  "Put contents of buffer stream into the page at offset; return the buffer-stream"
  (buffer-read-to-array-offset (page-buffer page) offset bs)
  bs)

(defmethod read-buffer-stream ((page buffer-page) (bs buffer-stream) offset length)
  "Put array contents at offset into buffer-stream and return stream"
  (declare (type fixnum offset length))
  (buffer-write-from-array-offset (page-buffer page) offset length bs)
  bs)
  
;;
;; Page-level IO with backing stream store
;;

(defmethod associate-page ((page buffer-page) (stream stream) position)
  (setf (page-position page) position)
  (setf (page-stream-store page) stream)
  page)

(defmethod seek-to-page ((page buffer-page))
  (file-position (page-stream-store page) (page-position page)))

(defmethod load-page ((page buffer-page))
  (seek-to-page page)
  (read-sequence (page-buffer page) (page-stream-store page)))

(defmethod flush-page ((page buffer-page))
  (seek-to-page page)
  (write-sequence (page-buffer page) (page-stream-store page)))

(defmethod zero-page ((page buffer-page) &optional (value 0))
  (loop for i from 0 upto (1- (length (page-buffer page))) do
        (setf (aref (page-buffer page) i) value))
  page)

;; ============================================================================
;;
;; Caching buffer pool
;;
;; ============================================================================

(defparameter *default-buffer-pool-pages* 4000)
(defparameter *default-page-size* 4096)

(defclass buffer-pool ()
  ((lock :accessor pool-lock :initarg :lock :initform nil)
   (page-count :accessor pool-pages :initarg :pages :initform *default-buffer-pool-pages*)
   (page-size :accessor pool-page-size :initarg :page-size :initform *default-page-size*)
   (free-list :accessor pool-free-list :initform nil)
   (active-list :accessor pool-active-list :initform nil)
   (least-recently-used :accessor pool-lru-page :initform nil)
   (hash :accessor pool-hash :initform nil)))

(defmethod initialize-instance :after ((pool buffer-pool) &rest rest)
  "Create a set of pages to populate the pool"
  (declare (ignore rest))
  (labels ((make-page ()
	     (make-instance 'buffer-page :page-size 
			    (pool-page-size pool))))
    (unless (= (pool-pages pool) 0)
      (setf (pool-free-list pool) (make-page)))
    (let ((prior (pool-free-list pool)))
      (dotimes (i (pool-pages pool) pool)
	(setf prior (link-node (make-page) prior nil))))))

;;
;; Pool level operations
;;

(defmethod eject-page ((pool buffer-pool))
  "Eject the least recently used, unwritten page, from the cache"
  (assert (not (null (pool-lru-page pool))))
  (let ((lru (pool-lru-page pool)))
    (setf (pool-lru-page pool) (dlist-prev (unlink-node lru)))
    (loop until (or (null lru) (not (page-dirty-p lru))) do
	 (setf lru (dlist-prev lru)))
    (when (null lru)
      (error "No unwritten pages available to eject!  Memory exhausted!"))
    lru))

(defun pop-free-list (pool)
  (let ((page (pool-free-list pool)))
    (setf (pool-free-list pool) (dlist-next page))
    (unlink-node page)))

(defun push-free-list (page pool)
  (link-node page nil (pool-free-list pool))
  (setf (pool-free-list pool) page))

(defun push-active-list (page pool)
  (link-node page nil (pool-active-list pool))
  (setf (pool-active-list pool) page))

(defun touch-page (page pool)
  (push-active-list (unlink-node page) pool))

(defmethod get-empty-page ((pool buffer-pool))
  (if (null (pool-free-list pool))
      (eject-page pool)
      (pop-free-list pool)))

(defmethod lookup-page ((pool buffer-pool) position stream)
  (let ((pages (gethash position (pool-hash pool))))
    (find stream pages :key #'page-stream-store)))

(defmethod cache-page ((pool buffer-pool) page)
  (push page (gethash (page-position page) (pool-hash pool))))

;; ------------------------------------------------------------------------
;;
;; User cache operations
;;
;; ------------------------------------------------------------------------

(defmethod get-page ((pool buffer-pool) position stream)
  (touch-page 
   (or (lookup-page pool position stream)
       (cache-page pool
		   (load-page 
		    (associate-page (get-empty-page pool) stream position))))
   pool))


