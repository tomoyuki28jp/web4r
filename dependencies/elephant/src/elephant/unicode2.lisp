;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;
;;; unicode2.lisp -- binary encoding/decoding for strings with codes > 8 bits
;;; 
;;; By Ian Eslick, <ieslick common-lisp net>
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

;;;
;;; In this file UTF8 means 8-bit < #x79 only for efficiency, UTF16
;;; means UTF16 format but only for values < #xFFFF.  In the odd cases
;;; of non-0 unicode planes we just use UTF-32 to avoid the time cost
;;; of translation.  Only when converting from a serialized UTF-X do
;;; we worry about encoding (UTF32->UTF16).  If an ascii/utf8 lisp
;;; encounters a UTF-16 we know it uses more than 127 values and thus
;;; cannot be represented in the reading lisp so we assert an error.
;;;

(in-package :elephant-serializer2)

(declaim #-elephant-without-optimize (optimize (speed 3) (safety 1) (space 0)))

;; 
;; Serialize string: simplify store by discovering utf8/utf16 and utf32; trade off
;; storage for computation time.  Unicode makes fast memcpy too complicated so we'll
;; go for simplicity and portability for now.
;;

(defun serialize-string (string bstream)
  "Try to write each format type and bail if code is too big"
  (declare (type buffer-stream bstream)
	   (type string string))
  (cond ((and (not (equal "" string)) (> (char-code (char string 0)) #xFFFF))
	 (serialize-to-utf32le string bstream))
	;; Accelerate the common case where a character set is not Latin-1
	((and (not (equal "" string)) (> (char-code (char string 0)) #xFF))
	 (or (serialize-to-utf16le string bstream)
	     (serialize-to-utf32le string bstream)))
	;; Actually code pages > 0 are rare; so we can pay an extra cost
	(t (or (serialize-to-utf8 string bstream)
	       (serialize-to-utf16le string bstream)
	       (serialize-to-utf32le string bstream)))))

(defun serialize-to-utf8 (string bstream)
  "Standard serialization"
  (declare (type buffer-stream bstream)
 	   (type string string))
  (elephant-memutil::with-struct-slots ((buffer buffer-stream-buffer)
					(size buffer-stream-size)
					(allocated buffer-stream-length))
      bstream
    (let* ((saved-size (buffer-stream-size bstream))
	   (saved-pos (elephant-memutil::buffer-stream-position bstream))
	   (characters (length string)))
      (labels ((fail () 
		 (setf (buffer-stream-size bstream) saved-size)
		 (setf (elephant-memutil::buffer-stream-position bstream) saved-pos)
		 (return-from serialize-to-utf8 nil))
	       (succeed ()
		 (return-from serialize-to-utf8 t)))
	(buffer-write-byte +utf8-string+ bstream)
	(buffer-write-int32 characters bstream)
	(let ((needed (+ size characters)))
	    (declare (type fixnum needed))
	    (when (> needed allocated)
	      (resize-buffer-stream bstream needed))
	    (etypecase string
	      (simple-string
	       (loop for i fixnum from 0 below characters do
		    (let ((code (char-code (schar string i))))
		      (declare (type fixnum code))
		      (when (> code #xFF) (fail))
		      (setf (uffi:deref-array buffer '(:array :unsigned-char) (+ i size)) code))))
	      (string
	       (loop for i fixnum from 0 below characters do 
		    (let ((code (char-code (char string i))))
		      (declare (type fixnum code))
		      (when (> code #xFF) (fail))
		      (setf (uffi:deref-array buffer '(:array :unsigned-char) (+ i size)) code)))))
	    (setf (buffer-stream-size bstream) needed)
	    (succeed))))))

(defun serialize-to-utf16le (string bstream)
  "Serialize to utf16le compliant format unless contains code pages > 0"
  (declare (type buffer-stream bstream)
 	   (type string string))
  (elephant-memutil::with-struct-slots ((buffer buffer-stream-buffer)
					(size buffer-stream-size)
					(allocated buffer-stream-length))
      bstream
      (let* ((saved-size (buffer-stream-size bstream))
	     (saved-pos (elephant-memutil::buffer-stream-position bstream))
	     (characters (length string)))
	(labels ((fail () 
		   (setf (buffer-stream-size bstream) saved-size)
		   (setf (elephant-memutil::buffer-stream-position bstream) saved-pos)
		   (return-from serialize-to-utf16le nil))
		 (succeed ()
		   (return-from serialize-to-utf16le t)))
	  (buffer-write-byte +utf16-string+ bstream)
	  (buffer-write-int32 characters bstream)
	  (let ((needed (+ size (* characters 2))))
	  (when (> needed allocated)
	    (resize-buffer-stream bstream needed))
	  (etypecase string
	    (simple-string
	     (loop for i fixnum from 0 below characters do
		  (let ((code (char-code (schar string i))))
		    (when (> code #xFFFF) (fail))
		    (setf (uffi:deref-array buffer '(:array :unsigned-char) (+ (* i 2) size))
;;			  (coerce (ldb (byte 8 8) code) '(signed 8)))
			  (ldb (byte 8 8) code))
		    (setf (uffi:deref-array buffer '(:array :unsigned-char) (+ (* i 2) size 1))
;;			  (coerce (ldb (byte 8 0) code) '(signed 8))))))
			  (ldb (byte 8 0) code)))))
	    (string
	     (loop for i fixnum from 0 below characters do 
		  (let ((code (char-code (schar string i))))
		    (when (> code #xFFFF) (fail))
		    (setf (uffi:deref-array buffer '(:array :unsigned-char) (+ (* i 2) size)) 
;;			  (coerce (ldb (byte 8 8) code) '(signed 8)))
			  (ldb (byte 8 8) code))
		    (setf (uffi:deref-array buffer '(:array :unsigned-char) (+ (* i 2) size 1))
;;			  (coerce (ldb (byte 8 0) code) '(signed 8)))))))
			  (ldb (byte 8 0) code))))))
	  (incf size (* characters 2))
	  (succeed))))))

(defun serialize-to-utf32le (string bstream)
  "Serialize to utf32 compliant format unless contains code pages > 0"
   (declare (type buffer-stream bstream)
	    (type string string))
  (elephant-memutil::with-struct-slots ((buffer buffer-stream-buffer)
					(size buffer-stream-size)
					(allocated buffer-stream-length))
      bstream
      (let* ((characters (length string)))
	  (buffer-write-byte +utf32-string+ bstream)
	  (buffer-write-int32 characters bstream)
	  (let ((needed (+ size (* 4 characters))))
	    (when (> needed allocated)
	      (resize-buffer-stream bstream needed))
	  (etypecase string
	    (simple-string
	     (loop for i fixnum from 0 below characters do
		  (let ((code (char-code (schar string i))))
		    (when (> code #x10FFFF) (error "Invalid unicode code type"))
		    (setf (uffi:deref-array buffer '(:array :unsigned-char) (+ (* i 4) size 0))
			  (ldb (byte 8 24) code))
		    (setf (uffi:deref-array buffer '(:array :unsigned-char) (+ (* i 4) size 1))
			  (ldb (byte 8 16) code))
		    (setf (uffi:deref-array buffer '(:array :unsigned-char) (+ (* i 4) size 2))
			  (ldb (byte 8 8) code))
		    (setf (uffi:deref-array buffer '(:array :unsigned-char) (+ (* i 4) size 3))
			  (ldb (byte 8 0) code)))))
	    (string
	     (loop for i fixnum from 0 below characters do 
		  (let ((code (char-code (schar string i))))
		    (when (> code #x10FFFF) (error "Invalid unicode code type"))
		    (setf (uffi:deref-array buffer '(:array :unsigned-char) (+ (* i 4) size 0))
			  (ldb (byte 8 24) code))
		    (setf (uffi:deref-array buffer '(:array :unsigned-char) (+ (* i 4) size 1))
			  (ldb (byte 8 16) code))
		    (setf (uffi:deref-array buffer '(:array :unsigned-char) (+ (* i 4) size 2))
			  (ldb (byte 8 8) code))
		    (setf (uffi:deref-array buffer '(:array :unsigned-char) (+ (* i 4) size 3))
			  (ldb (byte 8 0) code))))))
	  (incf size (* characters 4))
	  t))))

;;
;; Deserialization of Strings 
;; 

(defparameter native-string-type
  #+(and allegro ics) :utf16le
  #+(and allegro (not ics)) :utf8
  #+(and sbcl sb-unicode) :utf32le
  #+(and sbcl (not sb-unicode)) :utf8
  #+lispworks :utf16le
  #+openmcl :utf8
  )

(defun compatible-unicode-support-p (encoding-type)
  "This is a crude hack and can be improved later, but
   we assume if you have code pages > 0 you need or use
   a 32-bit encoding.  I'm assuming that 16-bit unicode
   supporting lisps only support code page 0 and do not
   use conjugate pair coding and variable length unicode
   string representations (formal utf-16)"
  (or (eq encoding-type :utf8) 
      (eq encoding-type native-string-type)
      (and (eq encoding-type :utf16le) (eq native-string-type :utf32le))))

(defgeneric deserialize-string (type bstream &optional temp-string))

(defmethod deserialize-string :around ((type t) bstream &optional temp-string)
  #+lispworks (coerce (call-next-method) 'lispworks:simple-text-string)
  #-lispworks (call-next-method))

(defmethod deserialize-string ((type (eql :utf8)) bstream &optional temp-string)
  (declare (type buffer-stream bstream))
  ;; Default char-code method
  (let* ((length (buffer-read-int32 bstream))
	 (pos (elephant-memutil::buffer-stream-position bstream)))
    (incf (elephant-memutil::buffer-stream-position bstream) length)
    (progn
      (let ((string (or temp-string (make-string length :element-type 'character))))
	(loop for i fixnum from 0 below length do
	     (setf (char string i)
		   (code-char (uffi:deref-array (buffer-stream-buffer bstream) 
						'(:array :unsigned-byte) 
						(+ pos i)))))
	(the simple-string string)))))

(defmethod deserialize-string ((type (eql :utf16le)) bstream &optional temp-string)
  "All returned strings are simple-strings for, uh, simplicity"
  (declare (type buffer-stream bstream))
  (let* ((length (buffer-read-int32 bstream))
	 (string (or temp-string (make-string length :element-type 'character)))
	 (pos (elephant-memutil::buffer-stream-position bstream))
	 (code 0))
    (macrolet ((next-byte (offset)
		 `(uffi:deref-array (buffer-stream-buffer bstream) '(:array :unsigned-byte) (+ (* i 2) pos ,offset))))
      (declare (type simple-string string)
	       (type fixnum length pos code))
      (assert (subtypep (type-of string) 'simple-string))
      (assert (compatible-unicode-support-p :utf16le))
      (loop for i fixnum from 0 below length do
	   (setf code (dpb (next-byte 0) (byte 8 8) 0))
	   (setf code (dpb (next-byte 1) (byte 8 0) code))
	   (setf (schar string i) (code-char code)))
      (incf (elephant-memutil::buffer-stream-position bstream)
	    (* length 2)))
    (the simple-string string)))

(defmethod deserialize-string ((type (eql :utf32le)) bstream  &optional temp-string)
  (declare (type buffer-stream bstream))
  (macrolet ((next-byte (offset)
	       `(uffi:deref-array (buffer-stream-buffer bstream) '(:array :unsigned-byte) (+ (* i 4) pos ,offset))))
  (let* ((length (buffer-read-int32 bstream))
	 (string (or temp-string (make-string length :element-type 'character)))
	 (pos (elephant-memutil::buffer-stream-position bstream))
	 (code 0))
    (declare (type string string)
	     (type fixnum length pos code))
    (assert (subtypep (type-of string) 'simple-string))
    (assert (compatible-unicode-support-p :utf32le))
    (loop for i fixnum from 0 below length do
	 (setf code (dpb (next-byte 0) (byte 8 24) 0))
	 (setf code (dpb (next-byte 1) (byte 8 16) code))
	 (setf code (dpb (next-byte 2) (byte 8 8) code))
	 (setf code (dpb (next-byte 3) (byte 8 0) code))
	 (setf (char string i) (code-char code)))
    (incf (elephant-memutil::buffer-stream-position bstream)
	  (* length 4))
    (the simple-string string))))


  
  

