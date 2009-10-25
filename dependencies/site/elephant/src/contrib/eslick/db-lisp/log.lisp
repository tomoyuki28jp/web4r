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
;; Simple logging facility to track operations
;;

(defparameter *default-log-page-size*)

(defclass binary-file-logger ()
  ((lock :accessor bflogger-lock :initarg :lock :initform (make-ele-lock))
   (filename :accessor bflogger-filename :initarg :filename)
   (binary-file :accessor bflogger-bfile :initform nil)
   (current-offset :accessor bflogger-offset :initarg :offset :initform 0)
   (operation-reader :accessor bflogger-ops :initarg :op-reader)))

(defmethod initialize-instance :after ((log binary-file-logger) &rest rest)
  (unless (bflogger-stream log)
    (setf (bflogger-bfile log) 
	  (open-binary-file (bflogger-filename log)))))

(defmethod bflogger-stream ((log binary-file-logger))
  (when (bflogger-bfile log)
    (binary-file-stream (bflogger-bfile log))))

;;
;; Error conditions on log operations
;;

(define-condition log-full ()
  ((filename :accessor log-full-filename :initarg :filename)
   (logger :accessor log-full-logger :logger)))

(define-condition operation-error (error)
  ((op :accessor operation-error-op :initarg :op)))

;;
;; Top-level user interface
;;

(defun open-log (path &key (max-bytes (expt 2 23)))
  (make-instance 'binary-file-logger :filename path))

(defmethod close-log ((log binary-file-logger))
  (when (bflogger-bfile log)
    (close-binary-file (bflogger-bfile log))))

;;
;; Record and play operations
;;

(defclass bflog-op ()
  ((operation-id :accessor bflog-op-id :initarg :op-id :initform nil)
   (file-offset :accessor bflog-op-offset :initarg :offset :initform nil)
   (payload :accessor bflog-op-payload :initarg :payload))
  (:documentation "A cooperative class for reading and writing data to logs
                   as well as replaying logged operations.  Intended as a
                   base class for users"))

(defclass end-of-log-op (bflog-op)
  ((operation-id :initform +eol-op+)))

;;
;; Payload API
;;

(defmethod unparse-payload ((op bflog-op) array offset)
  "Default method; assume payload is a byte-array and return it, otherwise
   base class should override and return an array"
  (bflog-op-payload op))

(defmethod unparse-payload :around ((op bflog-op) array )
  (let ((payload (call-next-method)))
    (assert (typep payload '(array (unsigned-byte 8))))
    payload))

(defmethod parse-payload ((op bflog-op) (array (array (unsigned-byte 8))) offset)
  (declare (type fixnum offset))
  (setf (bflog-op-payload op) array))

;;
;; User interface
;;

(defvar *log-temp-array* (make-array 10000 :element-type '(unsigned-byte 8) :fill-pointer t :adjustable t))

(defmethod write-operation ((op bflog-op) (log binary-file-logger))
  (let ((array *log-temp-array*))
    (with-ele-lock (bflogger-lock log)
      (write-integer-to-array (bflog-op-id op) array 0 1) ;; tag
      (parse-payload op array 4) ;; get payload starting after length field
      (let ((end (fill-pointer array))) ;; length of payload
	(write-integer-to-array (- end 5) 1 4) ;; write payload length
	(write-sequence array (bflogger-stream log) :end (fill-pointer array)) ;; dump to disk
	(setf (fill-pointer array) 0))
	(finish-output (bflogger-stream log))
	t)))

;;(defmethod read-operation ((log binary-file-logger))
;;  (read-sequence 


  
  