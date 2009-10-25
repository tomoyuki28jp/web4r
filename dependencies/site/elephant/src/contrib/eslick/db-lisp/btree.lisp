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

(defparameter *btree-page-size* 8192
  "The size of a btree page.  8192 leaves room for 32 
   key/value pairs (@ 256 bytes /ea)")
(defparameter *btree-cache-size* 2048
  "The number of cache pages to allocate (4k pages at 
   4k /ea - 16MB working set)")

;;
;; Data layout
;; - page types: index, leaf, blobs
;;

(defparameter *db-version* 1)

(defparameter *type-table* 
  '((0 . :free)
    (1 . :index)
    (2 . :leaf)
    (3 . :overflow)
    (#xFE . :root-as-leaf)
    (#xFF . :root)))

(defun get-type (id)
  (assert (<= id (caar (last *type-table*))))
  (cdr (assoc id *type-table*)))

(defun get-type-id (type-symbol)
  (loop for (id . symbol) in *type-table* do
        (when (eq type-symbol symbol)
	  (return id))
       finally (error "Invalid page type identifier")))

;;
;; Byte fields
;;

(defun write-field (field page integer)
  (write-integer integer page (first field) (second field)))

(defun read-field (field page)
  (read-integer page (first field) (second field)))

(defun write-field-default (field page)
  (write-field field page (third field)))

(defun verify-field-default (field page)
  (assert (= (third field) (read-field field page))))

(defmacro def-field (name (start length &optional (default nil)))
  `(defparameter ,name 
     (list ,start ,length ,default)))

(defmethod field-length (field)
  (second field))

(defmethod field-start (field)
  (first field))

;;
;; Field definitions
;;

(def-field +page-type+ (0 1))

(defun read-page-type (page)
  (get-type (read-field +page-type+ page)))

(defun write-page-type (page type)
  (write-field +page-type+ page (get-type-id type)))

(def-field +free-list-next+ (1 4 0))

(defconstant +root-key-start+ 23)
(def-field +root-version+ (1 1 *db-version*))
(def-field +root-reserved+ (2 8 #xDEADBEEFDEADBEEF))
(def-field +root-alloc-pointer+ (10 4 0))
(def-field +root-free-pointer+ (14 4 0))
(def-field +root-last-valid-byte+ (18 3 +root-key-start+))
(def-field +root-num-keys+ (21 2 0))

(defconstant +index-key-start+ 14)
(def-field +index-reserved+ (1 8 0))
(def-field +index-last-valid-byte+ (9 3 +index-key-start+))
(def-field +index-num-keys+ (12 2 0))

(defconstant +leaf-key-start+ 14)
(def-field +leaf-prev+ (1 4 0))
(def-field +leaf-next+ (5 4 0))
(def-field +leaf-last-valid-byte+ (9 3 +leaf-key-start+))
(def-field +leaf-num-keys+ (12 2 0))

(defun leaf-p (page)
  (or (eq (page-type page) :leaf)
      (eq (page-type page) :root-as-leaf)))

;;
;; Initializing btree page types
;;

(defun initialize-root-page (page)
  (write-page-type page (setf (page-type page) :root-as-leaf))
  (write-field-default +root-version+ page)
  (write-field-default +root-reserved+ page)
  (write-field-default +root-alloc-pointer+ page)
  (write-field-default +root-free-pointer+ page)
  (write-field-default +root-last-valid-byte+ page)
  (write-field-default +root-num-keys+ page))

(defun initialize-index-page (page)
  (write-page-type page (setf (page-type page) :index))
  (write-field-default +index-reserved+ page)
  (write-field-default +index-num-keys+ page))

(defun initialize-leaf-page (page)
  (write-page-type page (setf (page-type page) :leaf))
  (write-field-default +leaf-prev+ page)
  (write-field-default +leaf-next+ page))

(defun initialize-free-page (page)
  (write-page-type page (setf (page-type page) :free))
  (write-field-default +free-list-next+ page))

;;
;; Keys and values
;;

(defparameter *max-key-size* 255)
(defparameter *max-value-size* 255)

(defmethod read-pointer ((page buffer-page) offset)
  (read-integer page offset 4))

(defmethod write-pointer ((page buffer-page) offset pointer)
  (write-integer pointer page offset 4))

(defmethod extract-key ((page buffer-page) offset bs)
  (let ((klen (read-integer page offset 4)))
    (values (when (> klen 0) (read-buffer-stream page bs (+ offset 4) klen))
	    (read-pointer page (+ offset klen 4))
	    (+ klen 8))))

(defmethod write-key ((page buffer-page) offset (bs buffer-stream) pointer)
  (let ((klen (buffer-stream-size bs)))
    (assert (< klen *max-key-size*))
    (write-integer klen page offset)
    (write-buffer-stream page bs (+ offset 4))
    (write-pointer page (+ offset (buffer-stream-size bs) 4) pointer)))

(defmethod extract-value ((page buffer-page) offset (bs buffer-stream))
  (let ((vlen (read-integer page offset)))
    (values (when (> vlen 0) (read-buffer-stream page bs (+ offset 4) vlen)) vlen)))

(defmethod write-value ((page buffer-page) offset bs)
  (let ((vlen (buffer-stream-size bs)))
    (assert (< vlen *max-value-size*))
    (write-integer vlen page offset 4)
    (write-buffer-stream page bs (+ offset 4))))

(defmethod skip-value ((page buffer-page) offset)
  "Returns the offset after the value is consumed"
  (let ((vlen (read-integer page offset)))
    (+ offset vlen)))

(defun last-valid-byte (page)
  "Get the last valid page irrespective of page type"
  (case (page-type page)
    (:root (read-field +root-last-valid-byte+ page))
    (:root-as-leaf (read-field +root-last-valid-byte+ page))
    (:index (read-field +index-last-valid-byte+ page))
    (:leaf (read-field +leaf-last-valid-byte+ page))))

(defun set-last-valid-byte (page value)
  (case (page-type page)
    (:root (write-field +root-last-valid-byte+ page value))
    (:root-as-leaf (write-field +root-last-valid-byte+ page value))
    (:index (write-field +index-last-valid-byte+ page value))
    (:leaf (write-field +leaf-last-valid-byte+ page value))))

(defsetf last-valid-byte set-last-valid-byte)

(defun first-key-offset (page)
  (case (page-type page)
    (:root +root-key-start+)
    (:root-as-leaf +root-key-start+)
    (:index +index-key-start+)
    (:leaf +leaf-key-start+)))

(defmethod num-keys ((page buffer-page))
  (case (page-type page)
    (:root (read-field +root-num-keys+ page))
    (:root-as-leaf (read-field +root-num-keys+ page))
    (:index (read-field +index-num-keys+ page))
    (:leaf (read-field +leaf-num-keys+ page))))

(defmethod set-num-keys ((page buffer-page) value)
  (case (page-type page)
    (:root (write-field +root-num-keys+ page value))
    (:root-as-leaf (write-field +root-num-keys+ page value))
    (:index (write-field +index-num-keys+ page value))
    (:leaf (write-field +leaf-num-keys+ page value))))

(defsetf num-keys set-num-keys)

;;
;; Comparison functions
;;

(defun lexical-compare-< (bs1 bs2)
  "Stub comparison function"
  (if (= (buffer-stream-size bs1) (buffer-stream-size bs2))
      (loop for i from 0 below (buffer-stream-size bs1) do
	   (unless (element-equal bs1 bs2 i)
	     (return (if (element-< bs1 bs2 i) 
			 :less-than
			 :greater-than)))
	 finally (return :equal))
      (if (< (buffer-stream-size bs1) (buffer-stream-size bs2))
	  :less-than
	  :greater-than)))


(defun element-equal (bs1 bs2 offset)
  (= (deref-array (buffer-stream-buffer bs1) '(:array :unsigned-byte) offset)
     (deref-array (buffer-stream-buffer bs2) '(:array :unsigned-byte) offset)))

(defun element-< (bs1 bs2 offset)
  (< (deref-array (buffer-stream-buffer bs1) '(:array :unsigned-byte) offset)
     (deref-array (buffer-stream-buffer bs2) '(:array :unsigned-byte) offset)))

;;
;; BTREE Class and useful accessors
;;

(defclass lisp-btree ()
  ((pool :accessor btree-buffer-pool :initarg :pool
	 :documentation "Maintain a pool of memory pages")
   (primary-bfile :accessor btree-primary-file :initarg :bfile
	  :documentation "The file store for btrees")
   (root :accessor btree-root :initarg :root
	 :documentation "The in-memory root of main BTree DB")
   (compare-fn :accessor btree-compare-fn :initarg :compare-fn)))

(defmethod btree-stream ((bt lisp-btree))
  (binary-file-stream (btree-primary-file bt)))

(defmethod btree-get-page ((bt lisp-btree) position)
  (get-page (btree-buffer-pool bt) (btree-stream bt) position))

(defmethod btree-allocation-pointer ((bt lisp-btree))
  (read-field +root-alloc-pointer+ (btree-root bt)))

(defmethod write-btree-allocation-pointer (value (bt lisp-btree))
  (write-field +root-alloc-pointer+ (btree-root bt) value))

(defsetf btree-allocation-pointer write-btree-allocation-pointer)

(defmethod btree-free-pointer ((bt lisp-btree))
  (read-field +root-free-pointer+ (btree-root bt)))

(defmethod write-btree-free-pointer (value (bt lisp-btree))
  (write-field +root-alloc-pointer+ (btree-root bt) value))

(defsetf btree-free-pointer write-btree-free-pointer)


;;
;; Manipulating backing store
;;

;; Physical operations (not init, no flush)

(defmethod pop-free-db-page ((bt lisp-btree))
  "Take a page off the free list"
  (let* ((pop-page (btree-get-page bt (btree-free-pointer bt)))
	 (new-top-page (btree-get-page bt (read-field +free-list-next+ pop-page))))
    (setf (btree-free-pointer bt) (page-position new-top-page))
    pop-page))

(defmethod push-free-db-page ((bt lisp-btree) free-page)
  "Pushes an initialized (tagged) free page on the free list"
  (let ((new-top (page-position free-page))
	(old-top-page (btree-get-page bt (btree-free-pointer bt))))
    (write-field +free-list-next+ free-page old-top-page)
    (setf (btree-free-pointer bt) new-top)
    free-page))

(defmethod new-db-page ((bt lisp-btree))
  "Append a new page to the disk file"
  (let ((new-page-position (btree-allocation-pointer bt)))
    (incf (btree-allocation-pointer bt) 
	  (page-size (btree-root bt)))
    new-page-position))
			   
(defmethod get-free-db-page ((bt lisp-btree))
  "Get a fresh page from free list or by allocation"
  (if (> (btree-free-pointer bt) 0)
      (pop-free-db-page bt)
      (new-db-page bt)))

(defmethod leaf-next (page)
  "Access the next page field of a leaf"
  (read-field +leaf-next+ page))
(defmethod set-leaf-next (page pointer)
  (write-field +leaf-next+ page pointer))
(defsetf leaf-next set-leaf-next)

(defmethod set-leaf-prev (page pointer)
  "Access the prev page field of a leaf"
  (write-field +leaf-prev+ page pointer))
(defmethod leaf-prev (page)
  (read-field +leaf-prev+ page))
(defsetf leaf-prev set-leaf-prev)

;; Logical operations

(defmethod free-page ((bt lisp-btree) page)
  "Free a page so it goes on the free list"
  (initialize-free-page page)
  (push-free-db-page bt page))

(defmethod allocate-index-page ((bt lisp-btree))
  (let ((idx-page (get-free-db-page bt)))
    (initialize-index-page idx-page)
    idx-page))

(defmethod allocate-leaf-page ((bt lisp-btree))
  (let ((leaf-page (get-free-db-page bt)))
    (initialize-leaf-page leaf-page)
    leaf-page))

(defun insert-leaf-page (new-page new-pointer prev-page next-page)
  "Link in a leaf page from the double linked list of leaf pages"
  (setf (leaf-prev new-page) (leaf-prev next-page)
	(leaf-next new-page) (leaf-next prev-page)
	(leaf-next prev-page) new-pointer
	(leaf-prev next-page) new-pointer)
  new-page)

(defun delete-leaf-page (old-page)
  "Remove a leaf page from the double linked list of leaf pages"
  (setf (leaf-next (leaf-prev old-page)) (leaf-next old-page)
	(leaf-prev (leaf-next old-page)) (leaf-prev old-page)))


;;
;; Manipulating keys and values
;;

(defun insert-key (page start key-bs pointer)
  "Given a point just after a key/pointer or
   at the beginning of a key region, insert and
   copy the remaining data to make room checking
   for boundary conditions"
  (let* ((last-byte (last-valid-byte page))
	 (region-size (- last-byte start))
	 (length (buffer-stream-size key-bs))
	 (offset (+ length 8)))
    (assert (< (+ last-byte offset) (page-size page)))
    (assert (< offset 256))
    (copy-region page start region-size offset)
    (write-key page start key-bs pointer)
    (setf (last-valid-byte page) (+ offset last-byte))
    (incf (num-keys page))
    page))

(defun insert-key-and-value (page start key-bs pointer value-bs)
  (let* ((last-byte (last-valid-byte page))
	 (region-size (- last-byte start))
	 (length (+ (buffer-stream-size key-bs)
		    (buffer-stream-size value-bs)))
	 (offset (+ length 12)))
    (assert (< (+ last-byte offset) (page-size page)))
    (assert (< offset 256))
    (copy-region page start region-size offset)
    (write-key page start key-bs pointer)
    (write-value page (+ start 8) value-bs)
    (setf (last-valid-byte page) (+ offset last-byte))
    (incf (num-keys page))
    page))

(defun delete-key (page start)
  (let* ((last-byte (last-valid-byte page))
	 (key-size (read-integer page start))
	 (begin (+ start key-size 8))
	 (region-size (- last-byte begin))
	 (offset (- (+ key-size 8))))
    (copy-region page begin region-size offset)
    (setf (last-valid-byte page) (+ offset last-byte))
    (decf (num-keys page))
    page))

(defun delete-key-and-value (page start)
  (let* ((last-byte (last-valid-byte page))
	 (key-size (read-integer page start))
	 (value-size (read-integer page (+ start key-size 4)))
	 (delete-size (+ key-size value-size 12))
	 (begin (+ start delete-size))
	 (region-size (- last-byte begin))
	 (offset (- delete-size)))
    (copy-region page begin region-size offset)
    (setf (last-valid-byte page) (+ offset last-byte))
    (decf (num-keys page))
    page))

(defun replace-value (page vstart new-value)
  (let* ((last-byte (last-valid-byte page))
	 (old-value-size (read-integer page vstart))
	 (new-value-size (buffer-stream-size new-value))
	 (region-start (+ vstart old-value-size 4))
	 (region-length (- last-byte region-start))
	 (offset (- new-value-size old-value-size)))
    (unless (= 0 offset)
      (copy-region page region-start region-length offset))
    (write-value page vstart new-value)))

(defmacro scan-page-keys ((key-bs pointer position btree page) &body body)
  "Walks a page one key at a time returning the associated pointer and
   position after consuming the pointer.  For leaf pages, this places it
   at the beginning of the value field.  The body is not evaluated
   if there are zero keys."
  (declare (ignorable lisp-btree))
  (assert (and (atom key-bs) (atom pointer) (atom position)))
  (let ((i (gensym))
	(dbkey (gensym))
	(consumed (gensym)))
    `(loop 
	with ,position = (first-key-offset ,page)
	for ,i fixnum from 0 below (num-keys ,page) do
	  (reset-buffer-stream ,key-bs)
	  (multiple-value-bind (,dbkey ,pointer ,consumed)
	      (extract-key ,page ,position ,key-bs)
	    (declare (ignore ,dbkey))
	    (incf ,position ,consumed)
	    (progn
	      ,@body)
	    (when (leaf-p ,page)
	      (setf ,position (skip-value ,page ,position)))))))

;;
;; Top-level initialization
;;

(defun open-lisp-btree (path &key 
			(page-size *btree-page-size*) 
			(cache-pages *btree-cache-size*)
			bpool
			(compare-name 'lexical-compare-<)
			(if-does-not-exist :create)
			(if-exists :new-version))
  (let* ((new-p (or 
		 (eq if-exists :overwrite)
		 (not (probe-file path :follow-symlinks t))))
	 (bfile (make-instance 'binary-file 
			       :path path
			       :if-exists if-exists 
			       :if-does-not-exist if-does-not-exist))
	 (bpool (if bpool bpool
		    (make-instance 'buffer-pool 
				   :pages cache-pages
				   :page-size page-size)))
	 (root (make-instance 'buffer-page 
			      :type :root
			      :page-size page-size))
	 (btree (make-instance 'lisp-btree 
			       :root root
			       :pool bpool
			       :bfile bfile 
			       :compare-fn compare-name)))
    (associate-page root (binary-file-stream bfile) 0)
    (if new-p 
	(initialize-root-page root)
	(load-page root))
    (assert (root-p root))
    btree))

(defun close-lisp-btree (btree)
  (close-file (btree-primary-file btree))
  (setf (btree-buffer-pool btree) nil)
  (setf (btree-root btree) nil))

(defun root-p (page)
  (assert (= #xDEADBEEFDEADBEEF (read-field +root-reserved+ page)))
  (let ((type (read-page-type page)))
    (or (eq :root type)
	(eq :root-as-leaf type))))

(defmethod btree-key-compare ((bt btree) bs1 bs2)
  (funcall (symbol-function (btree-compare-fn bt)) bs1 bs2))

;;
;; Main Btree Operations
;;

(defun btree-search (btree key)
  "Finds value associated with key (buffer-stream) in btree or nil"
  (with-buffer-streams (testkey value)
    (multiple-value-bind (leaf-page position equalp)
	(find-value btree (btree-root btree) key testkey)
      (when equalp
	(extract-value leaf-page position value)))))

(defun find-value (btree page key test-bs)
  (scan-page-keys (test-bs nextpage position btree page)
    (let ((result (btree-key-compare btree key test-bs)))
      (if (not (leaf-p page))
	  (case result
	    (:less-than
	     (find-value btree nextpage key test-bs))
	    (:equal
	     (find-value btree nextpage key test-bs)))
	  (case result
	    (:less-than
	     (return-from find-value (values page position nil)))
	    (:equal 
	     (return-from find-value (values page position t))))))))

(defun btree-insert (btree key-bs value-bs &optional (overwritep t))
  (with-buffer-streams (testkey)
    (multiple-value-bind (leaf-page position equalp)
	(find-value btree (btree-root btree) key-bs testkey)
      (cond ((and equalp overwritep)
	     (replace-value leaf-page position value-bs))
	    ((not equalp)
	     (insert-key-and-value leaf-page position key-bs 0 value-bs))))))

;;(defun find-value-inserting (btree page key test-bs)
  ;; maintain tree balance as we go
;;  (if (> 
;;  (scan-page-keys (test-bs nextpage position btree page)
;;    (let ((result (btree-key-compare btree key test-bs)))
;;      (


;;(defun find-insertion-point
;;    (multiple-value-bind (leaf-page position 

;; btree-delete

;; Internal operations:
;; btree-split-child
;; btree-insert-nonfull

