;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;
;;; serializer.lisp -- convert Lisp data to/from byte arrays
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

(defpackage :elephant-serializer1
  (:use :cl :elephant :elephant-memutil)
  #+cmu
  (:import-from :bignum
		%bignum-ref)
  #+sbcl
  (:import-from :sb-bignum
		%bignum-ref)
  (:import-from #:elephant 
		get-cached-instance
		slot-definition-allocation
		slot-definition-name
		compute-slots
		slots-and-values
		oid
		int-byte-spec
		array-type-from-byte
	        byte-from-array-type
		database-version
		translate-and-intern-symbol))

(in-package :elephant-serializer1)

(declaim (inline int-byte-spec
		 serialize deserialize
		 slots-and-values
		 deserialize-bignum))

(uffi:def-type foreign-char :char)

;; Constants

(defconstant +fixnum+                1)
(defconstant +char+                  2)
(defconstant +single-float+          3)
(defconstant +double-float+          4)
(defconstant +negative-bignum+       5)
(defconstant +positive-bignum+       6)
(defconstant +rational+              7)

(defconstant +nil+                   8)

;; 8-bit
(defconstant +ucs1-symbol+           9)
(defconstant +ucs1-string+          10)
(defconstant +ucs1-pathname+        11)

;; 16-bit
(defconstant +ucs2-symbol+          12)
(defconstant +ucs2-string+          13)
(defconstant +ucs2-pathname+        14)

;; 32-bit
(defconstant +ucs4-symbol+          20)
(defconstant +ucs4-string+          21)
(defconstant +ucs4-pathname+        22)

(defconstant +persistent+           15) ;; stored by id+classname
(defconstant +cons+                 16)
(defconstant +hash-table+           17)
(defconstant +object+               18)
(defconstant +array+                19)

(defconstant +reserved-dbinfo+    #xF0)

(defconstant +fill-pointer-p+     #x40)
(defconstant +adjustable-p+       #x80)

(defvar *lisp-obj-id* 0 
  "Circularity ids for the serializer.")

(defvar *circularity-hash* (make-hash-table)
  "Circularity hash for the serializer.")

(defun clear-circularity-hash ()
  "This handles the case where we store an object with lots
   of object references.  CLRHASH then starts to dominate
   performance as it has to visit ever spot in the table so
   we're better off GCing the old table than clearing it"
  (declare  #-elephant-without-optimize (optimize (speed 3) (safety 0)))
  (if (> (hash-table-size *circularity-hash*) 100)
      (setf *circularity-hash* (make-hash-table :test 'eq :size 50))
      (clrhash *circularity-hash*)))

(defun serialize (frob bs sc)
  "Serialize a lisp value into a buffer-stream."
  (declare  #-elephant-without-optimize (optimize (speed 3) (safety 0))
	   (type buffer-stream bs)
	   (ignore sc))
  (setq *lisp-obj-id* 0)
  (clear-circularity-hash)
  (labels 
      ((%serialize (frob)
	 (declare  #-elephant-without-optimize (optimize (speed 3) (safety 0)))
	 (typecase frob
	   (fixnum
	    (buffer-write-byte +fixnum+ bs)
	    (buffer-write-int frob bs))
	   (null
	    (buffer-write-byte +nil+ bs))
	   (symbol
	    (let ((s (symbol-name frob)))
	      (declare (type string s) (dynamic-extent s))
	      (buffer-write-byte 
	       #+(and allegro ics)
	       (etypecase s
		 (base-string +ucs1-symbol+) ;; +ucs1-symbol+
		 (string +ucs2-symbol+))
	       #+(or (and sbcl sb-unicode) lispworks)
	       (etypecase s 
		 (base-string +ucs1-symbol+)
		 (string #+sbcl +ucs4-symbol+ #+lispworks +ucs2-symbol+))
	       #-(or lispworks (and allegro ics) (and sbcl sb-unicode))
	       +ucs1-symbol+
	       bs)
	      (buffer-write-int (byte-length s) bs)
	      (buffer-write-string s bs)
	      (let ((package (symbol-package frob)))
		(if package
		    (%serialize (package-name package))
		    (%serialize nil)))))
	   (string
	    (progn
	    (buffer-write-byte 
	     #+(and allegro ics)
	     (etypecase frob
	       (base-string +ucs1-string+)  ;; +ucs1-string+
	       (string +ucs2-string+))
	     #+(or (and sbcl sb-unicode) lispworks)
	     (etypecase frob
	       (base-string +ucs1-string+)
	       (string #+sbcl +ucs4-string+ #+lispworks +ucs2-string+))
	     #-(or lispworks (and allegro ics) (and sbcl sb-unicode))
	     +ucs1-string+
	     bs)
	    (buffer-write-int (byte-length frob) bs)
	    (buffer-write-string frob bs)))
	   (persistent
	    (buffer-write-byte +persistent+ bs)
	    (buffer-write-int (oid frob) bs)
	    ;; This circumlocution is necessitated by 
	    ;; an apparent bug in SBCL 9.9 --- type-of sometimes
	    ;; does NOT return the "proper name" of the class as the
	    ;; CLHS says it should, but gives the class object itself,
	    ;; which cannot be directly serialized....
	    (let ((tp (type-of frob)))
	      #+(or sbcl)
	      (if (not (symbolp tp))
		  (setf tp (class-name (class-of frob))))
	      (%serialize tp))
	      )
	   #-(and :lispworks (or :win32 :linux))
	   (single-float
	    (buffer-write-byte +single-float+ bs)
	    (buffer-write-float frob bs))
	   (double-float
	    (buffer-write-byte +double-float+ bs)
	    (buffer-write-double frob bs))
	   (character
	    (buffer-write-byte +char+ bs)
	    ;; might be wide!
	    (buffer-write-uint (char-code frob) bs))
	   (pathname
	    (let ((s (namestring frob)))
	      (declare (type string s) (dynamic-extent s))
	      (buffer-write-byte 
	       #+(and allegro ics) 
	       (etypecase s
		 (base-string +ucs1-pathname+) ;;  +ucs1-pathname+
		 (string +ucs2-pathname+))
	       #+(or (and sbcl sb-unicode) lispworks)
	       (etypecase s 
		 (base-string +ucs1-pathname+)
		 (string #+sbcl +ucs4-pathname+ #+lispwoks +ucs2-pathname+))
	       #-(or lispworks (and allegro ics) (and sbcl sb-unicode))
	       +ucs1-pathname+
	       bs)
	      (buffer-write-int (byte-length s) bs)
	      (buffer-write-string s bs)))
	   (integer
	    (let* ((num (abs frob))
		   (word-size (ceiling (/ (integer-length num) 32)))
		   (needed (* word-size 4)))
	      (declare (type fixnum word-size needed))
	      (if (< frob 0) 
		  (buffer-write-byte +negative-bignum+ bs)
		  (buffer-write-byte +positive-bignum+ bs))
	      (buffer-write-int needed bs)
	      (loop for i fixnum from 0 below word-size 
		    ;; this ldb is consing on CMUCL!
		    ;; there is an OpenMCL function which should work 
		    ;; and non-cons
		    do
		    #+(or cmu sbcl)
		    (buffer-write-uint (%bignum-ref num i) bs)
		    #+(or allegro lispworks openmcl)
		    (buffer-write-uint (ldb (int-byte-spec i) num) bs))))
	   (rational
	    (buffer-write-byte +rational+ bs)
	    (%serialize (numerator frob))
	    (%serialize (denominator frob)))
	   (cons
	    (buffer-write-byte +cons+ bs)
	    (let ((idp (gethash frob *circularity-hash*)))
	      (if idp (buffer-write-int idp bs)
		  (progn
		    (buffer-write-int (incf *lisp-obj-id*) bs)
		    (setf (gethash frob *circularity-hash*) *lisp-obj-id*)
		    (%serialize (car frob))
		    (%serialize (cdr frob))))))
	   (hash-table
	    (buffer-write-byte +hash-table+ bs)
	    (let ((idp (gethash frob *circularity-hash*)))
	      (if idp (buffer-write-int idp bs)
		  (progn
		    (buffer-write-int (incf *lisp-obj-id*) bs)
		    (setf (gethash frob *circularity-hash*) *lisp-obj-id*)
		    (%serialize (hash-table-test frob))
		    (%serialize (hash-table-rehash-size frob))
		    (%serialize (hash-table-rehash-threshold frob))
		    (%serialize (hash-table-count frob))
		    (loop for key being the hash-key of frob
			  using (hash-value value)
			  do 
			  (%serialize key)
			  (%serialize value))))))
	   (standard-object
	    (buffer-write-byte +object+ bs)
	    (let ((idp (gethash frob *circularity-hash*)))
	      (if idp (buffer-write-int idp bs)
		  (progn
		    (buffer-write-int (incf *lisp-obj-id*) bs)
		    (setf (gethash frob *circularity-hash*) *lisp-obj-id*)
		    (%serialize (type-of frob))
		    (let ((svs (slots-and-values frob)))
		      (declare (dynamic-extent svs))
		      (%serialize (/ (length svs) 2))
		      (loop for item in svs
			    do (%serialize item)))))))
	   (array
	    (buffer-write-byte +array+ bs)
	    (let ((idp (gethash frob *circularity-hash*)))
	      (if idp (buffer-write-int idp bs)
		  (progn
		    (buffer-write-int (incf *lisp-obj-id*) bs)
		    (setf (gethash frob *circularity-hash*) *lisp-obj-id*)
		    (buffer-write-byte 
		     (logior (byte-from-array-type (array-element-type frob))
			     (if (array-has-fill-pointer-p frob) 
				 +fill-pointer-p+ 0)
			     (if (adjustable-array-p frob) 
				 +adjustable-p+ 0))
		     bs)
		    (let ((rank (array-rank frob)))
		      (buffer-write-int rank bs)
		      (loop for i fixnum from 0 below rank
			    do (buffer-write-int (array-dimension frob i) 
						 bs)))
		    (when (array-has-fill-pointer-p frob)
		      (buffer-write-int (fill-pointer frob) bs))
		    (loop for i fixnum from 0 below (array-total-size frob)
			  do
			  (%serialize (row-major-aref frob i)))))))
	   )))
    (%serialize frob)
    bs))

(defparameter *trace-serializer* t)

(defparameter *tag-table*
  `((,+fixnum+ . "fixnum32")
    (,+char+ . "char")
    (,+single-float+ . "single-float")
    (,+double-float+ . "double float")
    (,+negative-bignum+ . "neg bignum")
    (,+positive-bignum+ . "pos bignum")
    (,+rational+ . "rational number")
    (,+nil+ . "null")
    (,+ucs1-symbol+ . "8-bit symbol")
    (,+ucs1-string+ . "8-bit string")
    (,+ucs1-pathname+ . "8-bit pathname")
    (,+ucs2-symbol+ . "16-bit symbol")
    (,+ucs2-string+ . "16-bit string")
    (,+ucs2-pathname+ . "16-bit pathname")
    (,+ucs4-symbol+ . "32-bit symbol")
    (,+ucs4-string+ . "32-bit string")
    (,+ucs4-pathname+ . "32-bit pathname")
    (,+persistent+ . "persistent object")
    (,+cons+ . "cons cell")
    (,+hash-table+ . "hash table")
    (,+object+ . "standard object")
    (,+array+ . "array")))

(defun enable-serializer-tracing ()
  (setf *trace-serializer* t))

(defun disable-serializer-tracing ()
  (setf *trace-serializer* nil))

(defun print-pre-deserialize-tag (tag)
  (when *trace-serializer*
    (let ((tag-name (assoc tag *tag-table*)))
      (if tag-name
	  (format t "Deserializing type: ~A~%" tag-name)
	  (progn
	    (format t "Unrecognized tag: ~A~%" tag)
	    (break))))))

(defun print-post-deserialize-tag (value)
  (when *trace-serializer*
    (format t "Returned: ~A~%" value)))

(defun deserialize (buf-str sc)
  "Deserialize a lisp value from a buffer-stream."
  (declare  #-elephant-without-optimize (optimize (speed 3) (safety 0))
	   (type (or null buffer-stream) buf-str))
  (labels 
      ((%deserialize (bs)
	 (declare  #-elephant-without-optimize (optimize (speed 3) (safety 0))
		  (type buffer-stream bs))
	 (let ((tag (buffer-read-byte bs)))
	   (declare (type foreign-char tag))
;;	   (print-pre-deserialize-tag tag)
;;	   (let ((value 
	   (cond
	     ((= tag +fixnum+) 
	      (buffer-read-fixnum bs))
	     ((= tag +nil+) nil)
	     ((= tag +ucs1-symbol+)
	      (let ((name (buffer-read-ucs1-string bs (buffer-read-fixnum bs)))
		    (maybe-package-name (%deserialize bs)))
		(translate-and-intern-symbol name maybe-package-name (database-version sc))))
	     #+(or lispworks (and allegro ics))
	     ((= tag +ucs2-symbol+)
	      (let ((name (buffer-read-ucs2-string bs (buffer-read-fixnum bs)))
		    (maybe-package-name (%deserialize bs)))
		(translate-and-intern-symbol name maybe-package-name (database-version sc))))
	     #+(and sbcl sb-unicode)
	     ((= tag +ucs4-symbol+)
	      (let ((name (buffer-read-ucs4-string bs (buffer-read-fixnum bs)))
		    (maybe-package-name (%deserialize bs)))
		(translate-and-intern-symbol name maybe-package-name (database-version sc))))
	     ((= tag +ucs1-string+)
	      (buffer-read-ucs1-string bs (buffer-read-fixnum bs)))
	     #+(or lispworks (and allegro ics))
	     ((= tag +ucs2-string+)
	      (buffer-read-ucs2-string bs (buffer-read-fixnum bs)))
	     #+(and sbcl sb-unicode)
	     ((= tag +ucs4-string+)
	      (buffer-read-ucs4-string bs (buffer-read-fixnum bs)))
	     ((= tag +persistent+)
;;	      (get-cached-instance *store-controller*
	      (get-cached-instance sc
				   (buffer-read-fixnum bs)
				   (%deserialize bs)))
	     ((= tag +single-float+)
	      (buffer-read-float bs))
	     ((= tag +double-float+)
	      (buffer-read-double bs))
	     ((= tag +char+)
	      (code-char (buffer-read-uint bs)))
	     ((= tag +ucs1-pathname+)
	      (parse-namestring 
	       (or (buffer-read-ucs1-string bs (buffer-read-fixnum bs)) "")))
	     #+(or lispworks (and allegro ics))
	     ((= tag +ucs2-pathname+)
	      (parse-namestring 
	       (or (buffer-read-ucs2-string bs (buffer-read-fixnum bs)) "")))
	     #+(and sbcl sb-unicode)
	     ((= tag +ucs4-pathname+)
	      (parse-namestring 
	       (or (buffer-read-ucs4-string bs (buffer-read-fixnum bs)) "")))
	     ((= tag +positive-bignum+) 
	      (deserialize-bignum bs (buffer-read-fixnum bs) t))
	     ((= tag +negative-bignum+) 
	      (deserialize-bignum bs (buffer-read-fixnum bs) nil))
	     ((= tag +rational+) 
	      (/ (the integer (%deserialize bs)) 
		 (the integer (%deserialize bs))))
	     ((= tag +cons+)
	      (let* ((id (buffer-read-fixnum bs))
		     (maybe-cons (gethash id *circularity-hash*)))
		(if maybe-cons maybe-cons
		    (let ((c (cons nil nil)))
		      (setf (gethash id *circularity-hash*) c)
		      (setf (car c) (%deserialize bs))
		      (setf (cdr c) (%deserialize bs))
		      c))))
	     ((= tag +hash-table+)
	      (let* ((id (buffer-read-fixnum bs))
		     (maybe-hash (gethash id *circularity-hash*)))
		(if maybe-hash maybe-hash
		    (let ((h (make-hash-table :test (%deserialize bs)
					      :rehash-size (%deserialize bs)
					      :rehash-threshold 
					      (%deserialize bs))))
		      (setf (gethash id *circularity-hash*) h)
		      (loop for i fixnum from 0 below (%deserialize bs)
			    do
			    (setf (gethash (%deserialize bs) h) 
				  (%deserialize bs)))
		      h))))
	     ((= tag +object+)
	      (let* ((id (buffer-read-fixnum bs))
		     (maybe-o (gethash id *circularity-hash*)))
		(if maybe-o maybe-o
		    (let ((typedesig (%deserialize bs)))
		      ;; now, depending on what typedesig is, we might 
		      ;; or might not need to specify the store controller here..
		      (let ((o 
			     (or (ignore-errors
				   (if (subtypep typedesig 'persistent)
				       (make-instance typedesig :sc sc)
				       ;; if the this type doesn't exist in our object
				       ;; space, we can't reconstitute it, but we don't want 
				       ;; to abort completely, we will return a special object...
				       ;; This behavior could be configurable; the user might 
				       ;; prefer an abort here, but I prefer surviving...
				       (make-instance typedesig)
				       )
				   )
				 (list 'uninstantiable-object-of-type typedesig)
				 )
			      ))
			(if (listp o)
			    o
			    (progn
			      (setf (gethash id *circularity-hash*) o)
			      (loop for i fixnum from 0 below (%deserialize bs)
				    do
				    (setf (slot-value o (%deserialize bs))
					  (%deserialize bs)))
			      o)))))))
	     ((= tag +array+)
	      (let* ((id (buffer-read-fixnum bs))
		     (maybe-array (gethash id *circularity-hash*)))
		(if maybe-array maybe-array
		    (let* ((flags (buffer-read-byte bs))
			   (a (make-array 
			       (loop for i fixnum from 0 below 
				     (buffer-read-int bs)
				     collect (buffer-read-int bs))
			       :element-type (array-type-from-byte 
					      (logand #x3f flags))
			       :fill-pointer (/= 0 (logand +fill-pointer-p+ 
							   flags))
			       :adjustable (/= 0 (logand +adjustable-p+ 
							 flags)))))
		      (when (array-has-fill-pointer-p a)
			(setf (fill-pointer a) (buffer-read-int bs)))
		      (setf (gethash id *circularity-hash*) a)
		      (loop for i fixnum from 0 below (array-total-size a)
			    do
			    (setf (row-major-aref a i) (%deserialize bs)))
		      a))))		    
	     (t (error "deserialize fubar!")))
;;	     (print-post-deserialize-tag value)
;;	     value)
	     )))
  (etypecase buf-str 
    (null (return-from deserialize nil))
    (buffer-stream
     (setq *lisp-obj-id* 0)
     (clear-circularity-hash)
     (%deserialize buf-str)))))

(defun deserialize-bignum (bs length positive)
  (declare  #-elephant-without-optimize (optimize (speed 3) (safety 0))
	   (type buffer-stream bs)
	   (type fixnum length)
	   (type boolean positive))
  (loop for i from 0 below (/ length 4)
	for byte-spec = (int-byte-spec i)
	with num of-type integer = 0 
	do
	(setq num (dpb (buffer-read-uint bs) byte-spec num))
	finally (return (if positive num (- num)))))




