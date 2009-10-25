
;; This file courtesy of ironclad crypto library by Nathan Froyd, see License at bottom

(in-package :db-lisp)

;;; portability definitions for a buffer-octet stream

(eval-when (:compile-toplevel :load-toplevel :execute)

(defvar *binary-input-stream-class*
  (quote
   #+sbcl sb-gray:fundamental-binary-input-stream
   #+openmcl ccl:fundamental-binary-input-stream
   #+cmu ext:fundamental-binary-input-stream
   #+allegro excl:fundamental-binary-input-stream
   #-(or sbcl openmcl cmu allegro) (error "octet streams not supported in this implementation")))

(defvar *binary-output-stream-class*
  (quote
   #+sbcl sb-gray:fundamental-binary-output-stream
   #+openmcl ccl:fundamental-binary-output-stream
   #+cmu ext:fundamental-binary-output-stream
   #+allegro excl:fundamental-binary-output-stream
   #-(or sbcl openmcl cmu allegro) (error "octet streams not supported in this implementation")))

;;; FIXME: how to do CMUCL support for this?
(defvar *stream-element-type-function*
  (quote
   #+sbcl sb-gray::stream-element-type
   #+openmcl cl:stream-element-type
   #+cmu cl:stream-element-type
   #+allegro excl::stream-element-type
   #-(or sbcl openmcl cmu allegro) (error "octet streams not supported in this implementation")))

(defvar *stream-read-byte-function*
  (quote
   #+sbcl sb-gray:stream-read-byte
   #+openmcl ccl:stream-read-byte
   #+cmu ext:stream-read-byte
   #+allegro excl:stream-read-byte
   #-(or sbcl openmcl cmu allegro) (error "octet streams not supported in this implementation")))

(defvar *stream-write-byte-function*
  (quote
   #+sbcl sb-gray:stream-write-byte
   #+openmcl ccl:stream-write-byte
   #+cmu ext:stream-write-byte
   #+allegro excl:stream-write-byte
   #-(or sbcl openmcl cmu allegro) (error "octet streams not supported in this implementation")))

;;; FIXME: would be nice to support STREAM-{READ,WRITE}-SEQUENCE, too.  The
;;; function name hacking is here, but the actual implementation (and
;;; possible arglist headaches) are not.
(defvar *stream-read-sequence-function*
  (quote
   #+sbcl sb-gray:stream-read-sequence
   #+openmcl ccl:stream-read-vector
   #+cmu ext:stream-read-sequence
   #+allegro excl:stream-read-sequence
   #-(or sbcl openmcl cmu allegro) (error "octet streams not supported in this implementation")))

(defvar *stream-write-sequence-function*
  (quote
   #+sbcl sb-gray:stream-write-sequence
   #+openmcl ccl:stream-write-vector
   #+cmu ext:stream-write-sequence
   #+allegro excl:stream-write-sequence
   #-(or sbcl openmcl cmu allegro) (error "octet streams not supported in this implementation")))

(defvar *stream-file-position-function*
  (quote
   #+allegro excl::g-file-position
   #-(or allegro) (warning "octet stream random access not supported in this implementation")))
)


;;; implementation via Gray streams

(defclass octet-stream ()
  ((buffer :accessor buffer :initarg :buffer :type (simple-array (unsigned-byte 8) (*)))
   (index :accessor index :initarg :index :type fixnum)))

;;; These could be specialized for particular implementations by hooking
;;; in directly to the "native" stream methods for the implementation.

;;; FIXME: how to do CMUCL support for this?
(defmethod #.*stream-element-type-function* ((stream octet-stream))
  '(unsigned-byte 8))

(defmethod #.*stream-file-position-function* ((stream octet-stream) &optional pos)
  (if pos
      (setf (index stream) pos)
      (index stream)))


;;; input streams

(defclass octet-input-stream (octet-stream #.*binary-input-stream-class*)
  ((end :accessor end :initarg :end :type fixnum)))

(defmethod #.*stream-read-byte-function* ((stream octet-input-stream))
  (let ((buffer (buffer stream))
        (index (index stream)))
    (declare (type (simple-array (unsigned-byte 8) (*)) buffer))
    (cond
      ((>= index (end stream)) :eof)
      (t
       (setf (index stream) (1+ index))
       (aref buffer index)))))

(defun make-octet-input-stream (buffer &optional (start 0) end)
  "As MAKE-STRING-INPUT-STREAM, only with octets instead of characters."
  (declare (type (simple-array (unsigned-byte 8) (*)) buffer)
           (type fixnum start)
           (type (or fixnum null) end))
  (let ((end (or end (length buffer))))
    (make-instance 'octet-input-stream
                   :buffer buffer :index start :end end)))


;;; output streams

(defclass octet-output-stream (octet-stream #.*binary-output-stream-class*)
  ((index :accessor index :initform 0 :type fixnum)))

(defmethod #.*stream-write-byte-function* ((stream octet-output-stream) integer)
  (declare (type (unsigned-byte 8) integer))
  (let* ((buffer (buffer stream))
         (length (length buffer))
         (index (index stream)))
    (declare (type (simple-array (unsigned-byte 8) (*)) buffer))
    (when (>= index (length buffer))
      (let ((new-buffer (make-array (* 2 length)
                                    :element-type '(unsigned-byte 8))))
        (declare (type (simple-array (unsigned-byte 8) (*)) new-buffer))
        (replace new-buffer buffer)
        (setf buffer new-buffer
              (buffer stream) new-buffer)))
    (setf (aref buffer index) integer
          (index stream) (1+ index))
    integer))

#+(or sbcl allegro)
(defmethod #.*stream-write-sequence-function* ((stream octet-output-stream) sequence &optional (start 0) end)
  (declare (type (simple-array (unsigned-byte 8) (*)) sequence))
  (let* ((buffer (buffer stream))
         (buffer-length (length buffer))
         (sequence-length (- (or end (length sequence)) start))
         (index (index stream)))
    (declare (type (simple-array (unsigned-byte 8) (*)) buffer))
    (when (> (+ index sequence-length) buffer-length)
      (let ((new-buffer (make-array (* 2 (+ index sequence-length))
                                    :element-type '(unsigned-byte 8))))
        (declare (type (simple-array (unsigned-byte 8) (*)) new-buffer))
        (replace new-buffer buffer)
        (setf buffer new-buffer
              (buffer stream) new-buffer)))
    (locally (declare (type (simple-array (unsigned-byte 8) (*)) buffer))
      (replace buffer sequence :start1 index
               :start2 start :end2 (or end (length sequence)))
      (setf (index stream) (+ index sequence-length))
      sequence)))

(defun get-output-stream-octets (stream)
  "As GET-OUTPUT-STREAM-STRING, only with an octet output-stream instead
of a string output-stream."
  (let ((buffer (buffer stream))
        (index (index stream)))
    (setf (index stream) 0)
    (subseq buffer 0 index)))

(defun make-octet-output-stream ()
  "As MAKE-STRING-OUTPUT-STREAM, only with octets instead of characters."
  (make-instance 'octet-output-stream
                 :buffer (make-array 128 :element-type '(unsigned-byte 8))))

(defclass octet-io-stream (octet-output-stream octet-input-stream)
  ((limit :accessor limit-p :initarg :limit)))

(defmethod #.*stream-write-byte-function* ((stream octet-io-stream) integer)
  "Do not extend io streams on overflow"
  (declare (type (unsigned-byte 8) integer))
  (let* ((buffer (buffer stream))
         (length (length buffer))
         (index (index stream))
	 (end (end stream)))
    (declare (type (simple-array (unsigned-byte 8) (*)) buffer))
    (when (>= index end)
      (if (limit-p stream)
	  (error "Cannot write off end of input/output stream")
	  (let ((new-buffer (make-array (* 2 length)
					:element-type '(unsigned-byte 8))))
	    (declare (type (simple-array (unsigned-byte 8) (*)) new-buffer))
	    (replace new-buffer buffer)
	    (setf buffer new-buffer
		  (buffer stream) new-buffer))))
    (setf (aref buffer index) integer
          (index stream) (1+ index))
    integer))

(defun make-octet-io-stream (buffer &optional (start 0) end (limit 'empty))
  (declare (type (simple-array (unsigned-byte 8) (*)) buffer)
	   (type fixnum start)
	   (type (or fixnum null) end))
  (let ((end (or end (length buffer)))
	(limit (if (eq limit 'empty)
		   t
		   limit)))
    (make-instance 'octet-io-stream
		   :buffer buffer :index start :end end :limit limit)))
		 

;; Copyright (c) 2004, Nathan Froyd
;; All rights reserved.

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are
;; met:

;; * Redistributions of source code must retain the above copyright notice,
;;   this list of conditions and the following disclaimer.

;; * Redistributions in binary form must reproduce the above copyright
;;   notice, this list of conditions and the following disclaimer in the
;;   documentation and/or other materials provided with the distribution.

;; * Neither the name of Nathan Froyd nor the names of contributors to this
;;   software may be used to endorse or promote products derived from this
;;   software without specific prior written permission.

;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
;; IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
;; TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
;; PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
;; OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
;; EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
;; PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;; PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;; LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
