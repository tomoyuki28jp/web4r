;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;
;;; memutil.lisp -- FFI interface to UFFI/memory as base for serializer.lisp
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
;;; Elephant users are granted the rights to distribute and use this software
;;; as governed by the terms of the Lisp Lesser GNU Public License
;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.
;;;

(defpackage elephant-memutil
  (:documentation "A low-level UFFI-based memory access and
    serialization toolkit.  Provides basic cross-platform
    binary serialization support for backends.")
  (:use common-lisp uffi elephant-utils)
  #+cmu
  (:use alien)
  #+sbcl
  (:use sb-alien)
  #+cmu
  (:import-from :sys
		#:sap+)
  #+sbcl
  (:import-from :sb-sys
		#:sap+)  
  #+openmcl
  (:import-from :ccl
		#:byte-length)
  (:export    
	   #:buffer-stream #:make-buffer-stream #:with-buffer-streams
	   #:resize-buffer-stream #:resize-buffer-stream-no-copy 
	   #:reset-buffer-stream #:buffer-stream-buffer 
	   #:buffer-stream-length #:buffer-stream-size

	   #:buffer-read-to-array-offset #:buffer-write-from-array-offset

	   #:buffer-write-byte #:buffer-write-float 
	   #:buffer-write-double #:buffer-write-string 
           #:buffer-write-int32 #:buffer-write-uint32
           #:buffer-write-int64 #:buffer-write-uint64
           #:buffer-write-int #:buffer-write-uint

	   #:buffer-read-byte #:buffer-read-fixnum 
	   #:buffer-read-fixnum32 #:buffer-read-fixnum64
	   #:buffer-read-int #:buffer-read-uint
	   #:buffer-read-int32 #:buffer-read-uint32
	   #:buffer-read-int64 #:buffer-read-uint64
	   #:buffer-read-float #:buffer-read-double 

	   #:buffer-write-oid #:buffer-read-oid

	   #:buffer-read-ucs1-string
	   #+(or lispworks (and allegro ics)) #:buffer-read-ucs2-string 
	   #+(and sbcl sb-unicode) #:buffer-read-ucs4-string 
	   #:byte-length #:little-endian-p
	   
	   #:pointer-int #:pointer-void #:array-or-pointer-char
	   +NULL-CHAR+ +NULL-VOID+

	   #:*c-library-extension*
	   ))

(in-package "ELEPHANT-MEMUTIL")

#+cmu
(eval-when (:compile-toplevel)
  (proclaim '(optimize (ext:inhibit-warnings 3))))

(eval-when (:compile-toplevel :load-toplevel)
  (def-type pointer-int (* :int))
  (def-type pointer-void :pointer-void)
  (def-foreign-type array-or-pointer-char
      #+(or allegro) (:array :unsigned-char)
      #+(or cmu sbcl scl openmcl lispworks) (* :unsigned-char))
  (def-type array-or-pointer-char array-or-pointer-char)

  ;; Standard utility for copying two foreign buffers -- 
  ;;   also to test that lib is actually loaded!
  (def-function ("copy_buf" copy-bufs)
      ((dest array-or-pointer-char)
       (dest-offset :int)
       (src array-or-pointer-char)
       (src-offset :int)
       (length :int))
    :returning :void))

(eval-when (:compile-toplevel)
  (declaim 
   #-elephant-without-optimize (optimize (speed 3) (safety 1) (space 0) (debug 0))
   (inline read-int read-uint read-float read-double 
	   write-int write-uint write-float write-double
	   offset-char-pointer copy-str-to-buf %copy-str-to-buf copy-bufs
	   ;; resize-buffer-stream 
	   ;; buffer-stream-buffer buffer-stream-size buffer-stream-position
	   ;; buffer-stream-length 
	   buffer-write-oid buffer-read-oid
	   reset-buffer-stream
	   buffer-write-byte 
	   buffer-write-int32 buffer-write-uint32
	   buffer-write-int64 buffer-write-uint64
	   buffer-write-float buffer-write-double buffer-write-string
	   buffer-read-byte buffer-read-fixnum buffer-read-int32
	   buffer-read-uint32 buffer-read-int64 buffer-read-uint64
           buffer-read-float buffer-read-double buffer-read-ucs1-string
	   #+(or lispworks (and allegro ics)) buffer-read-ucs2-string
	   #+(and sbcl sb-unicode) buffer-read-ucs4-string))
  )

;; Constants and Flags
;; eventually write a macro which generates a custom flag function.

(defvar +NULL-VOID+ (make-null-pointer :void)
  "A null pointer to a void type.")
(defvar +NULL-CHAR+ (make-null-pointer :char)
  "A null pointer to a char type.")

;;
;; Thread local storage (special variables)
;;

(defvar *buffer-streams* (make-array 0 :adjustable t :fill-pointer t)
  "Vector of buffer-streams, which you can grab / return.")

(defvar *buffer-streams-lock* (ele-make-fast-lock))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; buffer-streams
;;;
;;; a stream-like interface for our buffers; methods are
;;; below.  ultimately we might want a gray / simple -stream
;;; for real, for now who cares?

(defstruct buffer-stream
  "A stream-like interface to foreign (alien) char buffers."
  (buffer (allocate-foreign-object :unsigned-char 10) :type array-or-pointer-char)
  (size 0 :type fixnum)
  (position 0 :type fixnum)
  (length 10 :type fixnum))

(defun grab-buffer-stream ()
  "Grab a buffer-stream from the *buffer-streams* resource pool."
  (if (= (length *buffer-streams*) 0)
      (make-buffer-stream)
      (ele-with-fast-lock (*buffer-streams-lock*)
	(vector-pop *buffer-streams*))))

(defun return-buffer-stream (bs)
  "Return a buffer-stream to the *buffer-streams* resource pool."
  (reset-buffer-stream bs)
  (ele-with-fast-lock (*buffer-streams-lock*)
    (vector-push-extend bs *buffer-streams*)))

(defmacro with-buffer-streams (names &body body)
  "Grab a buffer-stream, executes forms, and returns the
stream to the pool on exit."
  `(let ,(loop for name in names collect (list name '(grab-buffer-stream)))
    (unwind-protect
	 (progn ,@body)
      (progn
	,@(loop for name in names 
		collect (list 'return-buffer-stream name))))))

;;
;; Buffer management / pointer arithmetic
;;

;; Notes: on Allegro: with-cast-pointer + deref-array is
;; faster than FFI + C pointer arithmetic.  however pointer
;; arithmetic is usually consing.  OpenMCL supports
;; non-consing pointer arithmentic though.  Check these
;; CMUCL / SBCL things don't cons unless necessary.

;; TODO: #+openmcl versions which do macptr arith.  

#+(or cmu sbcl)
(defun read-int32 (buf offset)
  "Read a 32-bit signed integer from a foreign char buffer."
  (declare (type (alien (* unsigned-char)) buf)
	   (type fixnum offset))
  (the (signed-byte 32)
    (deref (cast (sap-alien (sap+ (alien-sap buf) offset) (* unsigned-char))
		 (* (signed 32))))))

#+(or cmu sbcl)
(defun read-int64 (buf offset)
  "Read a 64-bit signed integer from a foreign char buffer."
  (declare (type (alien (* unsigned-char)) buf)
	   (type fixnum offset))
  (the (signed-byte 64)
    (deref (cast (sap-alien (sap+ (alien-sap buf) offset) (* unsigned-char))
		 (* (signed 64))))))

#+(or cmu sbcl)
(defun read-uint32 (buf offset)
  "Read a 32-bit unsigned integer from a foreign char buffer."
  (declare (type (alien (* unsigned-char)) buf)
	   (type fixnum offset))
  (the (unsigned-byte 32)
    (deref (cast (sap-alien (sap+ (alien-sap buf) offset) (* unsigned-char))
		 (* (unsigned 32))))))


#+(or cmu sbcl)
(defun read-uint64 (buf offset)
  "Read a 64-bit unsigned integer from a foreign char buffer."
  (declare (type (alien (* unsigned-char)) buf)
	   (type fixnum offset))
  (the (signed-byte 64)
    (deref (cast (sap-alien (sap+ (alien-sap buf) offset) (* unsigned-char))
		 (* (signed 64))))))

#+(or cmu sbcl)
(defun read-float (buf offset)
  "Read a single-float from a foreign char buffer."
  (declare (type (alien (* unsigned-char)) buf)
	   (type fixnum offset))
  (the single-float
    (deref (cast (sap-alien (sap+ (alien-sap buf) offset) (* unsigned-char))
		 (* single-float)))))

#+(or cmu sbcl)
(defun read-double (buf offset)
  "Read a double-float from a foreign char buffer."
  (declare (type (alien (* unsigned-char)) buf)
	   (type fixnum offset))
  (the double-float
    (deref (cast (sap-alien (sap+ (alien-sap buf) offset) (* unsigned-char))
		 (* double-float)))))

#+(or cmu sbcl)
(defun write-int32 (buf num offset)
  "Write a 32-bit signed integer to a foreign char buffer."
  (declare (type (alien (* unsigned-char)) buf)
	   (type (signed-byte 32) num)
	   (type fixnum offset))
  (setf (deref (cast (sap-alien (sap+ (alien-sap buf) offset) (* unsigned-char))
		     (* (signed 32)))) num))

#+(or cmu sbcl)
(defun write-int64 (buf num offset)
  "Write a 64-bit signed integer to a foreign char buffer."
  (declare (type (alien (* unsigned-char)) buf)
	   (type (signed-byte 64) num)
	   (type fixnum offset))
  (setf (deref (cast (sap-alien (sap+ (alien-sap buf) offset) (* unsigned-char))
		     (* (signed 64)))) num))

#+(or cmu sbcl)
(defun write-uint32 (buf num offset)
  "Write a 32-bit unsigned integer to a foreign char buffer."
  (declare (type (alien (* unsigned-char)) buf)
	   (type (unsigned-byte 32) num)
	   (type fixnum offset))
  (setf (deref (cast (sap-alien (sap+ (alien-sap buf) offset) (* unsigned-char))
		     (* (unsigned 32)))) num))

#+(or cmu sbcl)
(defun write-uint64 (buf num offset)
  "Write a 64-bit unsigned integer to a foreign char buffer."
  (declare (type (alien (* unsigned-char)) buf)
	   (type (unsigned-byte 64) num)
	   (type fixnum offset))
  (setf (deref (cast (sap-alien (sap+ (alien-sap buf) offset) (* unsigned-char))
		     (* (unsigned 64)))) num))
#+(or cmu sbcl)
(defun write-float (buf num offset)
  "Write a single-float to a foreign char buffer."
  (declare (type (alien (* unsigned-char)) buf)
	   (type single-float num)
	   (type fixnum offset))
  (setf (deref (cast (sap-alien (sap+ (alien-sap buf) offset) (* unsigned-char))
		     (* single-float))) num))

#+(or cmu sbcl)
(defun write-double (buf num offset)
  "Write a double-float to a foreign char buffer."
  (declare (type (alien (* unsigned-char)) buf)
	   (type double-float num)
	   (type fixnum offset))
  (setf (deref (cast (sap-alien (sap+ (alien-sap buf) offset) (* unsigned-char))
		     (* double-float))) num))

#+(or cmu sbcl)
(defun offset-char-pointer (p offset)
  "Pointer arithmetic."
  (declare (type (alien (* unsigned-char)) p)
	   (type fixnum offset))
  (sap-alien (sap+ (alien-sap p) offset) (* unsigned-char)))

#-(or cmu sbcl)
(def-function ("read_int32" read-int32)
    ((buf array-or-pointer-char)
     (offset :int))
  :returning :int)

#-(or cmu sbcl)
(def-function ("read_uint32" read-uint32)
    ((buf array-or-pointer-char)
     (offset :int))
  :returning :unsigned-int)

#-(or cmu sbcl)
(def-function ("read_int64" read-int64)
    ((buf array-or-pointer-char)
     (offset :int))
  :returning :long)

#-(or cmu sbcl)
(def-function ("read_uint64" read-uint64)
    ((buf array-or-pointer-char)
     (offset :int))
  :returning :unsigned-long)

#-(or cmu sbcl)
(def-function ("read_float" read-float)
    ((buf array-or-pointer-char)
     (offset :int))
  :returning :float)

#-(or cmu sbcl)
(def-function ("read_double" read-double)
    ((buf array-or-pointer-char)
     (offset :int))
  :returning :double)

#-(or cmu sbcl)
(def-function ("write_int32" write-int32)
    ((buf array-or-pointer-char)
     (num :int)
     (offset :int))
  :returning :void)

#-(or cmu sbcl)
(def-function ("write_uint32" write-uint32)
    ((buf array-or-pointer-char)
     (num :unsigned-int)
     (offset :int))
  :returning :void)

#-(or cmu sbcl)
(def-function ("write_int64" write-int64)
    ((buf array-or-pointer-char)
     (num :long)
     (offset :int))
  :returning :void)

#-(or cmu sbcl)
(def-function ("write_uint64" write-uint64)
    ((buf array-or-pointer-char)
     (num :unsigned-long)
     (offset :int))
  :returning :void)

#-(or cmu sbcl)
(def-function ("write_float" write-float)
    ((buf array-or-pointer-char)
     (num :float)
     (offset :int))
  :returning :void)

#-(or cmu sbcl)
(def-function ("write_double" write-double)
    ((buf array-or-pointer-char)
     (num :double)
     (offset :int))
  :returning :void)

#-(or cmu sbcl)
(def-function ("offset_charp" offset-char-pointer)
    ((p array-or-pointer-char)
     (offset :int))
  :returning array-or-pointer-char)

;; Allegro and Lispworks use 16-bit unicode characters
#+(or cmu sbcl allegro lispworks)
(defmacro byte-length (s)
  "Return the number of bytes of the internal representation
of a string."
  #+(and allegro ics)
  ;; old: 
  ;; `(let ((l (length ,s))) (+ l l))
  `(etypecase ,s
     (base-string (length ,s)) ;; fast 0.6.1 
     (string (excl:native-string-sizeof ,s :external-format :unicode)))
  ;; (excl:native-string-sizeof ,s :external-format :unicode))
  #+(or (and sbcl sb-unicode) lispworks)
  `(etypecase ,s 
    (base-string (length ,s))
    (string (* (length ,s) #+sbcl 4 #+lispworks 2)))
  #-(or lispworks (and allegro ics) (and sbcl sb-unicode))
  `(length ,s))

;; for copying the bytes of a string to a foreign buffer
;; memcpy is faster than looping!  For Lispworks this causes
;; a string to array conversion, but I don't know how to do
;; any better (fli:replace-foreign-array is promising?)
#-(or cmu sbcl scl openmcl allegro)
(def-function ("copy_buf" copy-str-to-buf)
    ((dest array-or-pointer-char)
     (dest-offset :int)
     (src array-or-pointer-char)
     (src-offset :int)
     (length :int))
  :returning :void)

#+(or cmu sbcl scl)
(def-function ("copy_buf" %copy-str-to-buf)
    ((dest array-or-pointer-char)
     (dest-offset :int)
     (src array-or-pointer-char)
     (src-offset :int)
     (length :int))
  :returning :void)

#+(or cmu sbcl scl)
(defun copy-str-to-buf (d do s so l)
   (declare (type array-or-pointer-char d)
	    (type fixnum do so l)
	    (type string s))
   (%copy-str-to-buf d do 
		     #+sbcl
		     (sb-sys:vector-sap s) 
		     #+(or cmu scl)
		     (sys:vector-sap s) 
		     so l))

;; but OpenMCL can't directly pass string bytes.
#+openmcl
(defun copy-str-to-buf (dest dest-offset src src-offset length)
  "Copy a string to a foreign buffer.  From Gary Byers."
  (declare (type string src)
	   (type array-or-pointer-char dest)
	   (type fixnum length src-offset dest-offset)
	   (dynamic-extent src dest length))
  (multiple-value-bind (ivector disp)
      (ccl::array-data-and-offset src)
    (ccl::%copy-ivector-to-ptr ivector (+ disp src-offset)
			       dest dest-offset length)))

;; #+allegro
;; (defun copy-str-to-buf (dest dest-offset src src-offset length)
;;   "Use build-in unicode handling and copying facilities.
;;    NOTE: We need to validate the speed of this vs. default."
;;   (declare 
;; 	   (type string src)
;; 	   (type array-or-pointer-char dest)
;; 	   (type fixnum length src-offset dest-offset)
;; 	   (dynamic-extent src dest length))
;;   (excl:string-to-native (subseq src src-offset) :address (offset-char-pointer dest dest-offset)
;; 			     :external-format :unicode))

;; Lisp version, for kicks.  this assumes 8-bit chars!
#+(not (or cmu sbcl scl openmcl lispworks))
(defun copy-str-to-buf (dest dest-offset src src-offset length)
  "Copy a string to a foreign buffer."
   (declare (type string src)
	    (type array-or-pointer-char dest)
	    (type fixnum length src-offset dest-offset)
	    (dynamic-extent src dest length))
  (typecase src
    (simple-string
     (loop for i fixnum from 0 below length
	   do
	   (setf (deref-array dest '(:array :unsigned-char) (+ i dest-offset)) 
		 (char-code (schar src (+ i src-offset))))))
    (string
     (loop for i fixnum from 0 below length
	   do
	   (setf (deref-array dest '(:array :unsigned-char) (+ i dest-offset)) 
		 (char-code (char src (+ i src-offset))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; buffer-stream functions

(eval-when (:compile-toplevel :load-toplevel)
  (defun process-struct-slot-defs (slot-defs struct)
    (loop for def in slot-defs
	  collect (list (first def) (list (second def) struct)))))

(defmacro with-struct-slots (slot-defs struct &body body)
  `(symbol-macrolet ,(process-struct-slot-defs slot-defs struct)
    ,@body))

(defun resize-buffer-stream (bs length)
  "Resize the underlying buffer of a buffer-stream, copying the old data."
  (declare (type buffer-stream bs)
	   (type fixnum length))
  (with-struct-slots ((buf buffer-stream-buffer)
		      (size buffer-stream-size)
		      (len buffer-stream-length))
    bs		      
    (when (> length len)
      (let ((newlen (max length (* len 2))))
	(declare (type fixnum newlen))
	(let ((newbuf (allocate-foreign-object :unsigned-char newlen)))
	  ;; technically we just need to copy from position to size.....
	  (when (null-pointer-p newbuf)
	    (error "Failed to allocate buffer stream of length ~A.  allocate-foreign-object returned a null pointer" newlen))
	  (copy-bufs newbuf 0 buf 0 size)
	  (free-foreign-object buf)
	  (setf buf newbuf)
	  (setf len newlen)
	  nil)))))

(defun resize-buffer-stream-no-copy (bs length)
  "Resize the underlying buffer of a buffer-stream."
  (declare (type buffer-stream bs)
	   (type fixnum length))
  (with-struct-slots ((buf buffer-stream-buffer)
		      (size buffer-stream-size)
		      (len buffer-stream-length))
    bs		      
    (when (> length len)
      (let ((newlen (max length (* len 2))))
	(declare (type fixnum newlen))
	(let ((newbuf (allocate-foreign-object :unsigned-char newlen)))
	  (when (null-pointer-p newbuf)
	    (error "Failed to allocate buffer stream of length ~A.  allocate-foreign-object returned a null pointer" newlen))
	  (free-foreign-object buf)
	  (setf buf newbuf)
	  (setf len newlen)
	  nil)))))

(defun reset-buffer-stream (bs)
  "'Empty' the buffer-stream."
  (declare (type buffer-stream bs))
  (setf (buffer-stream-size bs) 0)
  (setf (buffer-stream-position bs) 0))

(defun buffer-write-byte (b bs)
  "Write a byte."
  (declare (type buffer-stream bs)
	   (type (unsigned-byte 8) b))
  (with-struct-slots ((buf buffer-stream-buffer)
		      (size buffer-stream-size)
		      (len buffer-stream-length))
    bs		      
    (let ((needed (+ size 1)))
      (when (> needed len)
	(resize-buffer-stream bs needed))
      (setf (deref-array buf '(:array :unsigned-char) size) b)
      (setf size needed))))

(defun buffer-write-int32 (i bs)
  "Write a 32-bit signed integer."
  (declare (type buffer-stream bs)
	   (type (signed-byte 32) i))
  (with-struct-slots ((buf buffer-stream-buffer)
		      (size buffer-stream-size)
		      (len buffer-stream-length))
    bs		      
    (let ((needed (+ size 4)))
      (when (> needed len)
	(resize-buffer-stream bs needed))
      (write-int32 buf i size)
      (setf size needed)
      nil)))

(defun buffer-write-uint32 (u bs)
  "Write a 32-bit unsigned integer."
  (declare (type buffer-stream bs)
	   (type (unsigned-byte 32) u))
  (with-struct-slots ((buf buffer-stream-buffer)
		      (size buffer-stream-size)
		      (len buffer-stream-length))
    bs		      
    (let ((needed (+ size 4)))
      (when (> needed len)
	(resize-buffer-stream bs needed))
      (write-uint32 buf u size)
      (setf size needed)
      nil)))

(defun buffer-write-int64 (i bs)
  "Write a 64-bit signed integer."
  (declare (type buffer-stream bs)
	   (type (signed-byte 64) i))
  (with-struct-slots ((buf buffer-stream-buffer)
		      (size buffer-stream-size)
		      (len buffer-stream-length))
    bs		      
    (let ((needed (+ size 8)))
      (when (> needed len)
	(resize-buffer-stream bs needed))
      (write-int64 buf i size)
      (setf size needed)
      nil)))

(defun buffer-write-uint64 (u bs)
  "Write a 64-bit unsigned integer."
  (declare (type buffer-stream bs)
	   (type (unsigned-byte 64) u))
  (with-struct-slots ((buf buffer-stream-buffer)
		      (size buffer-stream-size)
		      (len buffer-stream-length))
    bs		      
    (let ((needed (+ size 8)))
      (when (> needed len)
	(resize-buffer-stream bs needed))
      (write-uint64 buf u size)
      (setf size needed)
      nil)))

(defun buffer-write-float (d bs)
  "Write a single-float."
  (declare (type buffer-stream bs)
	   (type single-float d))
  (with-struct-slots ((buf buffer-stream-buffer)
		      (size buffer-stream-size)
		      (len buffer-stream-length))
    bs		      
    (let ((needed (+ size 4)))
      (when (> needed len)
	(resize-buffer-stream bs needed))
      (write-float buf d size)
      (setf size needed)
      nil)))

(defun buffer-write-double (d bs)
  "Write a double-float."
  (declare (type buffer-stream bs)
	   (type double-float d))
  (with-struct-slots ((buf buffer-stream-buffer)
		      (size buffer-stream-size)
		      (len buffer-stream-length))
    bs		      
    (let ((needed (+ size 8)))
      (when (> needed len)
	(resize-buffer-stream bs needed))
      (write-double buf d size)
      (setf size needed)
      nil)))

(defun buffer-write-string (s bs)
  "Write the underlying bytes of a string.  On Unicode
   Lisps, this is a 16-bit operation."
  (declare (type buffer-stream bs)
	   (type string s))
  (with-struct-slots ((buf buffer-stream-buffer)
		      (size buffer-stream-size)
		      (len buffer-stream-length))
    bs		      
    (let* ((str-bytes (byte-length s))
	   (needed (+ size str-bytes)))
      (declare (type fixnum str-bytes needed)
	       (dynamic-extent str-bytes needed))
      (when (> needed len)
	(resize-buffer-stream bs needed))
;; I wonder if the basic problem here is that we are using this
;; routine instead of something like "copy-ub8-from-system-area"?
      #-allegro
      (copy-str-to-buf buf size s 0 str-bytes)
      #+allegro
      (etypecase s
	(base-string 
	 (copy-str-to-buf buf size s 0 str-bytes)) ;; v0.6.0 
	(string
	 (excl:string-to-native s :address (offset-char-pointer buf size) :external-format :unicode)
	 ))
      (setf size needed)
      nil)))

(defun buffer-read-byte (bs)
  "Read a byte."
  (declare (type buffer-stream bs))
  (let ((position (buffer-stream-position bs)))
    (incf (buffer-stream-position bs))
    (deref-array (buffer-stream-buffer bs) '(:array :unsigned-char) position)))


(defun buffer-read-byte-vector (bs)
   "Read the whole buffer into  byte vector."
   (declare (type buffer-stream bs))
   (let* ((position (buffer-stream-position bs))
 	(size (buffer-stream-size bs))
 	(vlen (- size position)))
     (if (>= vlen 0)
 	(let ((v (make-array vlen :element-type '(unsigned-byte 8))))
 	  (dotimes (i vlen v) 
 	      (setf (aref v i) (buffer-read-byte bs))))
 	nil)))
 
(defun buffer-write-byte-vector (bv bs)
   "Read the whole buffer into  byte vector."
   (declare (type buffer-stream bs))
   (let* ((position (buffer-stream-position bs))
 	 (size (buffer-stream-size bs))
 	 (vlen (length bv))
 	 (writable (max vlen (- size position))))
 	  (dotimes (i writable bs) 
 	      (buffer-write-byte (aref bv i) bs))))

(defun buffer-read-to-array-offset (arry offset bs)
  "Buffer relative; read contents of buffer-stream and write them into array at offset"
  (declare (type buffer-stream bs)
	   (type fixnum offset))
  (let* ((position (buffer-stream-position bs))
	 (size (buffer-stream-size bs))
	 (vlen (- size position)))
    (assert (< (+ offset size) (length arry)))
    (if (>= vlen 0)
	(dotimes (i vlen arry)
	  (setf (aref arry (+ i offset))
		(buffer-read-byte bs))))))

(defun buffer-write-from-array-offset (arry offset length bs)
  "Buffer relative; write array contents into buffer stream"
  (declare (type fixnum offset)
	   (type buffer-stream bs))
  (dotimes (i length arry)
    (buffer-write-byte (aref arry (+ i offset)) bs)))
  

;;
;; Compatibility
;;

(defun buffer-write-oid (i bs)
  (buffer-write-int32 i bs))

(defun buffer-read-oid (bs)
  (buffer-read-fixnum32 bs))

;;
;; Legacy support
;;

(defun buffer-read-int (bs)
  ;; deprecated, better to use explicit int32 or int64 version
  (buffer-read-int32 bs))

(defun buffer-read-fixnum (bs)
  ;; deprecated, better to use explicit int32 or int64 version
  (the fixnum (buffer-read-fixnum32 bs)))

(defun buffer-write-int (int bs)
  ;; deprecated, better to use explicit int32 or int64 version
  (buffer-write-int32 int bs))

(defun buffer-read-uint (bs)
  ;; deprecated, better to use explicit int32 or int64 version
  (buffer-read-uint32 bs))

(defun buffer-write-uint (int bs)
  ;; deprecated, better to use explicit int32 or int64 version
  (buffer-write-uint32 int bs))
  
(defconstant +2^32+ 4294967296)
(defconstant +2^64+ 18446744073709551616)

(defun buffer-read-fixnum32 (bs)
  "Read a 32-bit signed integer, which is assumed to be a fixnum."
  (declare (type buffer-stream bs))
  (let ((position (buffer-stream-position bs)))
    (setf (buffer-stream-position bs) (+ position 4))
    (the fixnum (read-int32 (buffer-stream-buffer bs) position))))

(defun buffer-read-int32 (bs)
  "Read a 32-bit signed integer."
  (declare (type buffer-stream bs))
  (let ((position (buffer-stream-position bs)))
    (setf (buffer-stream-position bs) (+ position 4))
    (the (signed-byte 32) (read-int32 (buffer-stream-buffer bs) position))))

(defun buffer-read-uint32 (bs)
  "Read a 32-bit unsigned integer."
  (declare (type buffer-stream bs))
  (let ((position (buffer-stream-position bs)))
    (setf (buffer-stream-position bs) (+ position 4))
    (the (unsigned-byte 32)(read-uint32 (buffer-stream-buffer bs) position))))

(defun buffer-read-fixnum64 (bs)
  (declare (type buffer-stream bs))
  (let ((position (buffer-stream-position bs)))
    (setf (buffer-stream-position bs) (+ position 8))
    (if (< #.most-positive-fixnum +2^32+)
	;; 32-bit or less fixnums; need to process as bignums
	(let ((first (read-int32 (buffer-stream-buffer bs) position))
	      (second (read-int32 (buffer-stream-buffer bs) (+ position 4))))
	  (if (little-endian-p)
	      (+ first (ash second 32))
	      (+ second (ash first 32))))
	;; Native 64-bit fixnums (NOTE: issues with non 32/64 bit fixnums?)
	(the fixnum (read-int64 (buffer-stream-buffer bs) position)))))

(defun buffer-read-int64 (bs)
  "Read a 64-bit signed integer."
  (declare (type buffer-stream bs))
  (let ((position (buffer-stream-position bs)))
    (setf (buffer-stream-position bs) (+ position 8))
    (the (signed-byte 64) (read-int64 (buffer-stream-buffer bs) position))))

(defun buffer-read-uint64 (bs)
  "Read a 64-bit unsigned integer."
  (declare (type buffer-stream bs))
  (let ((position (buffer-stream-position bs)))
    (setf (buffer-stream-position bs) (+ position 8))
    (the (unsigned-byte 64) (read-uint64 (buffer-stream-buffer bs) position))))

(defun buffer-read-float (bs)
  "Read a single-float."
  (declare (type buffer-stream bs))
  (let ((position (buffer-stream-position bs)))
    (setf (buffer-stream-position bs) (+ position 4))
    (read-float (buffer-stream-buffer bs) position)))

(defun buffer-read-double (bs)
  "Read a double-float."
  (declare (type buffer-stream bs))
  (let ((position (buffer-stream-position bs)))
    (setf (buffer-stream-position bs) (+ position 8))
    (read-double (buffer-stream-buffer bs) position)))

;; A non-back-compatible change was made in SBCL 8 moving to SBCL 9,
;; in that the function copy-from-system-area disappeared.
;; This code is an attempt to allow compilation under bothe SBCL 8 and SBCL 9.
;; Thanks to Juho Snellman for this idiom.
(eval-when (:compile-toplevel)
  (defun new-style-copy-p ()
    #+(and sbcl sb-unicode)
    (if (find-symbol "COPY-UB8-FROM-SYSTEM-AREA" "SB-KERNEL")
	'(:and) 
	'(:or))
    #-(and sbcl sb-unicode)
    t))

(defun buffer-read-ucs1-string (bs byte-length)
  "Read a UCS1 string."
  (declare (type buffer-stream bs)
	   (type fixnum byte-length))
  (let ((position (buffer-stream-position bs)))
    (setf (buffer-stream-position bs) (+ position byte-length))
    #-(and sbcl sb-unicode)
    (convert-from-foreign-string 
     (offset-char-pointer (buffer-stream-buffer bs) position) 
     :length byte-length :null-terminated-p nil)
    #+(and sbcl sb-unicode)
    (let ((res (make-string byte-length :element-type 'base-char)))
      #+#.(elephant-memutil::new-style-copy-p)
      (sb-kernel:copy-ub8-from-system-area 
        (sb-alien:alien-sap (buffer-stream-buffer bs))
        position
        res 
	0
        byte-length)
      #-#.(elephant-memutil::new-style-copy-p)
      (sb-kernel:copy-from-system-area 
       (sb-alien:alien-sap (buffer-stream-buffer bs))
       (* position sb-vm:n-byte-bits)
       res 
       (* sb-vm:vector-data-offset sb-vm:n-word-bits)
       (* byte-length sb-vm:n-byte-bits))
      res)))

#+(or lispworks (and allegro ics))
(defun buffer-read-ucs2-string (bs byte-length)
  "Read a UCS2 string."
  (declare (type buffer-stream bs)
	   (type fixnum byte-length))
  (let ((position (buffer-stream-position bs)))
    (setf (buffer-stream-position bs) (+ position byte-length))
    ;; wide!!!
    #+(and allegro ics)
    (excl:native-to-string 
     (offset-char-pointer (buffer-stream-buffer bs) position)
     :length byte-length
     :external-format :unicode)
    #+lispworks
    (fli:convert-from-foreign-string 
     (offset-char-pointer (buffer-stream-buffer bs) position)
     :length byte-length :external-format :unicode :null-terminated-p nil)))

#+(and sbcl sb-unicode)
(defun buffer-read-ucs4-string (bs byte-length)
  "Read a UCS4 string."
  (declare (type buffer-stream bs)
	   (type fixnum byte-length))
  (let ((position (buffer-stream-position bs)))
    (setf (buffer-stream-position bs) (+ position byte-length))
    (let ((res (make-string (/ byte-length 4) :element-type 'character)))
      #+#.(elephant-memutil::new-style-copy-p)
       (sb-kernel:copy-ub8-from-system-area 
        (sb-alien:alien-sap (buffer-stream-buffer bs))
        position 
        res 
	0
        byte-length)
       #-#.(elephant-memutil::new-style-copy-p)
      (sb-kernel:copy-from-system-area 
       (sb-alien:alien-sap (buffer-stream-buffer bs))
       (* position sb-vm:n-byte-bits)
       res 
       (* sb-vm:vector-data-offset sb-vm:n-word-bits)
       (* byte-length sb-vm:n-byte-bits))
      res)))

;;
;; What kind of machine are we on?
;;

(defparameter +little-endian+ nil)

(defun little-endian-p ()
  #+(or :x86 :x86-64 :LITTLE-ENDIAN) t
  #+(or :PPC :POWERPC :BIG-ENDIAN) nil
  #-(or :x86 :x86-64 :LITTLE-ENDIAN :PPC :POWERPC :BIG-ENDIAN)
  (progn
    (unless +little-endian+
      (with-buffer-streams (bs)
	(buffer-write-int32 #x1 bs)
	(if (= 0 (buffer-read-byte bs))
	    (setf +little-endian+ 2)
	    (setf +little-endian+ 1))))
    (if (eq +little-endian+ 1) t nil)))
	    

