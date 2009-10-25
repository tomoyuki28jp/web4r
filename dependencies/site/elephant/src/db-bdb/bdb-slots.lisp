;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;
;;; bdb-slots.lisp -- Implement the slot protocol
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

(in-package :db-bdb)

;;
;; Persistent slot protocol implementation
;;

(declaim #-elephant-without-optimize (optimize (speed 3) (safety 1) (space 0)))

(defmethod persistent-slot-reader ((sc bdb-store-controller) instance name)
  (with-buffer-streams (key-buf value-buf)
    (buffer-write-int (oid instance) key-buf)
    (serialize name key-buf sc)
    (let ((buf (db-get-key-buffered (controller-db sc)
				    key-buf value-buf
				    :transaction (my-current-transaction sc))))
      (if buf (deserialize buf sc)
	  #+cmu
	  (error 'unbound-slot :instance instance :slot name)
	  #-cmu
	  (error 'unbound-slot :instance instance :name name)))))

(defmethod persistent-slot-writer ((sc bdb-store-controller) new-value instance name)
  (with-buffer-streams (key-buf value-buf)
    (buffer-write-int (oid instance) key-buf)
    (serialize name key-buf sc)
    (serialize new-value value-buf sc)
    (db-put-buffered (controller-db sc)
		     key-buf value-buf
		     :transaction (my-current-transaction sc))
    new-value))

(defmethod persistent-slot-boundp ((sc bdb-store-controller) instance name)
  (with-buffer-streams (key-buf value-buf)
    (buffer-write-int (oid instance) key-buf)
    (serialize name key-buf sc)
    (let ((buf (db-get-key-buffered (controller-db sc)
				    key-buf value-buf
				    :transaction (my-current-transaction sc))))
      (if buf t nil))))

(defmethod persistent-slot-makunbound ((sc bdb-store-controller) instance name)
  (with-buffer-streams (key-buf)
    (buffer-write-int (oid instance) key-buf)
    (serialize name key-buf sc)
    (db-delete-buffered (controller-db sc) key-buf
			:transaction (my-current-transaction sc))))


