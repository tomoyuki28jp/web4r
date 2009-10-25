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

(in-package :elephant)

(defun serialize (frob bs sc)
  "Generic interface to serialization that dispatches based on the 
   current Elephant version"
  (assert sc)
  (funcall (symbol-function (controller-serialize sc)) frob bs sc))

(defun deserialize (bs sc)
  "Generic interface to serialization that dispatches based on the 
   current Elephant version"
  (assert sc)
  (funcall (symbol-function (controller-deserialize sc)) bs sc))

;;
;; Special structure support
;;

(defgeneric struct-constructor (class)
  (:documentation "Called to get the constructor name for a struct class.  Users
                  should overload this when they want to serialize non-standard
                  constructor names.  The default constructor make-xxx will work by 
                  default.  The argument is an eql style type: i.e. of type (eql 'my-struct)"))

(defmethod struct-constructor ((class t))
  (symbol-function (intern (concatenate 'string "MAKE-" (symbol-name class))
			   (symbol-package class))))

;;
;; SQL encoding support
;;

(defun serialize-to-base64-string (x sc)
  "Encode object using the store controller's serializer format,
   but encoded in a base64"
  (with-buffer-streams (out-buf)
  (cl-base64::usb8-array-to-base64-string
   (elephant-memutil::buffer-read-byte-vector 
    (serialize x out-buf sc)))
 )
)

(defun convert-buffer-to-base64-string (bs sc)
  (declare (ignore sc))
  (cl-base64::usb8-array-to-base64-string
   (elephant-memutil::buffer-read-byte-vector bs)
)
)

(defun deserialize-from-base64-string (x sc)
  "Decode a base64-string using the store controller's deserialize method"
  (with-buffer-streams (other)
    (deserialize 
     (elephant-memutil::buffer-write-byte-vector 
      (cl-base64::base64-string-to-usb8-array x)
      other 
      )
     sc)
    ))

(defun convert-buffer-from-base64-string (string sc)
  (with-buffer-streams (target)
    (deserialize 
     (elephant-memutil::buffer-write-byte-vector 
      target
      (cl-base64::base64-string-to-usb8-array string))
     sc)))

;;
;; Serializer independant system information
;;
;; We'll can this for now, can expose as API for backend later

(defconstant +reserved-dbinfo+ #xF0)
  
(defconstant +elephant-version+ 1)
(defconstant +elephant-serializer-version+ 2)

;; Database Version (a list of integers = [version major minor])

(defun serialize-database-version-key (bs)
  "Given a buffer-stream, encode a key indicating the version using
   the constant +elephant-version+"
  (serialize-reserved-tag bs)
  (serialize-system-tag +elephant-version+ bs))

(defun serialize-database-version-value (version bs)
  "Serializes a list containing three integers to the buffer stream bs"
  (assert (and (= (length version) 3)))
  (destructuring-bind (version major minor) version
    (serialize-system-integer version bs)
    (serialize-system-integer major bs)
    (serialize-system-integer minor bs)))

(defun deserialize-database-version-value (bs)
  "Deserializes the 3 integer list from buffer stream bs"
  (let ((version (deserialize-system-integer bs))
	(major (deserialize-system-integer bs))
	(minor (deserialize-system-integer bs)))
    (list version major minor)))

;;
;; Serializer version (so you know what encoding is/was used in the db)
;;

(defun serialize-database-serializer-version-key (bs)
  (serialize-reserved-tag bs)
  (serialize-system-tag +elephant-serializer-version+ bs))

(defun serialize-database-serializer-version-value (version bs)
  (serialize-system-integer version bs))

(defun deserialize-database-serializer-version-value (bs)
  (deserialize-system-integer bs))

;; Simple API for basic byte and integer operations

(defun serialize-reserved-tag (bs)
  (elephant-memutil::buffer-write-byte +reserved-dbinfo+ bs))

(defun serialize-system-tag (byte bs)
  (elephant-memutil::buffer-write-byte byte bs))

(defun serialize-system-integer (int bs)
  (elephant-memutil::buffer-write-int32 int bs))
(defun deserialize-system-integer (bs)
  (elephant-memutil::buffer-read-int32 bs))

    
;; (defclass blob ()
;;   ((slot1 :accessor slot1 :initarg :slot1)
;;    (slot2 :accessor slot2 :initarg :slot2)))

;; (defvar keys (loop for i from 1 to 1000 
;; 		   collect (concatenate 'string "key-" (prin1-to-string i))))

;; (defvar objs (loop for i from 1 to 1000
;; 		   collect (make-instance 'blob
;; 					  :slot1 i
;; 					  :slot2 (* i 100))))
;; (defmethod blob-equal ((a blob) (b blob))
;;   (and (equal (slot1 a) (slot1 b))
;;        (equal (slot2 a) (slot2 b))))

;; (defun test-base64-serializer ()
;;   (let* ((x1 "spud")
;; 	 (x2 (cons 'a 'b))
;; 	 (objs (loop for i from 1 to 1000
;; 		   collect (make-instance 'blob
;; 					  :slot1 i
;; 					  :slot2 (* i 100))))
;; 	 )
;;     (and
;;      (ser-deser-equal x1)
;;      (ser-deser-equal x2)
;;      (reduce 
;;       #'(lambda (x y) (and  x y))
;;       (mapcar 
;;        #'(lambda (x) 
;; 		 (equal x 
;; 			(with-buffer-streams (other)
;; 			  (deserialize (serialize x other))
;; 			  )))
;; ;;			(deserialize-from-base64-string 
;; ;;			 (serialize-to-base64-string x))))
;;        objs)  
;;      :initial-value t)
;;      )))

;;;;
;;;; Common utilities
;;;;

(defun slots-and-values (o)
  "List of slot names followed by values for object"
  (loop for sd in (compute-slots (class-of o))
	for slot-name = (slot-definition-name sd)
	with ret = ()
	do
	(when (and (slot-boundp o slot-name)
		   (eq :instance
		       (slot-definition-allocation sd)))
	  (push (slot-value o slot-name) ret)
	  (push slot-name ret))
	finally (return ret)))

(defun struct-slots-and-values (object)
  "List of slot names followed by values for structure object"
  (let ((result nil)
	(slots 
	  #+(or sbcl cmu allegro)
	  (mapcar #'slot-definition-name (class-slots (class-of object)))
	  #+openmcl
	  (let* ((sd (gethash (class-name (class-of object)) ccl::%defstructs%))
		 (slots (if sd (ccl::sd-slots sd))))
	    (mapcar #'car (if (symbolp (caar slots)) slots (cdr slots))))
	  #+lispworks
	  (structure:structure-class-slot-names (class-of object))))
    (loop for slot in slots do
	 (push (slot-value object slot) result)
	 (push slot result))
    result))
	 
;; array type tags

(declaim (type hash-table array-type-to-byte byte-to-array-type))
(defvar array-type-to-byte (make-hash-table :test 'equalp))
(defvar byte-to-array-type (make-hash-table :test 'equalp))

(setf (gethash 'T array-type-to-byte) #x00)
(setf (gethash 'base-char array-type-to-byte) #x01)
(setf (gethash 'character array-type-to-byte) #x02)
(setf (gethash 'single-float array-type-to-byte) #x03)
(setf (gethash 'double-float array-type-to-byte) #x04)
(setf (gethash '(complex single-float) array-type-to-byte) #x05)
(setf (gethash '(complex double-float) array-type-to-byte) #x06)
(setf (gethash 'fixnum array-type-to-byte) #x07)
(setf (gethash 'bit array-type-to-byte) #x08)

(defun type= (t1 t2)
  (and (subtypep t1 t2) (subtypep t2 t1)))

(let ((counter 8))
  (loop for i from 2 to 65
	for spec = (list 'unsigned-byte i)
	for uspec = (upgraded-array-element-type spec)
	when (type= spec uspec)
	do
	(setf (gethash spec array-type-to-byte) (incf counter)))
  (loop for i from 2 to 65
	for spec = (list 'signed-byte i)
	for uspec = (upgraded-array-element-type spec)
	when (type= spec uspec)
	do
	(setf (gethash spec array-type-to-byte) (incf counter))))

(loop for key being the hash-key of array-type-to-byte 
      using (hash-value value)
      do
      (setf (gethash value byte-to-array-type) key))

(defun array-type-from-byte (b)
  (gethash b byte-to-array-type))

(defun byte-from-array-type (ty)
  (the (unsigned-byte 8) (gethash ty array-type-to-byte)))

(defun int-byte-spec (position)
  "Shared byte-spec peformance hack; not thread safe so removed
   from use for serializer2"
  (declare (type (unsigned-byte 24) position))
  (byte 32 (* 32 position)))

